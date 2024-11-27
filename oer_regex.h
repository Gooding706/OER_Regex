#ifndef OER_INCLUDE_REGEX_H
#define OER_INCLUDE_REGEX_H
#include <stdbool.h>
// privately used by nfa nodes
typedef struct
{
    char acceptable;
    void *next; // we declare this as a void pointer although it is only ever dereferenced as node
    unsigned char flag;
    void *data;
} connection;

// privately used by nfa regex to create nfa
typedef struct
{
    connection *connections;

    unsigned short numConnections; // limits the number of connectons to the 16 bit unsigned integer maximum
    bool acceptingNode;
} node;

// used to store nfa
typedef struct
{
    node **content;
    int length;
    int capacity;
} nodeArray;

// used to hold captured items
typedef struct
{
    int capacity;
    int length;
    char **content;
} stringList;

// our compiled regular expression
typedef struct
{
    nodeArray *nodes;
    node *startNode;
} regex_t;

typedef struct
{
    stringList capturedStrings;
    char *readEnd;
    bool success;
} regexReturn_t;

regex_t compileExpression(const char *expression);

regexReturn_t findFirst(regex_t *expression, const char *input);
regexReturn_t findAll(regex_t *expression, const char *input);

void freeRegex(regex_t *expr);
void freeStringList(stringList list);

#endif

#ifdef OER_REGEX_IMPLEMENTATION

#include <stdlib.h>
#include <stdio.h>
#include <memory.h>
#include <ctype.h>

// maximums
#define MAXOPERATORSTACK 256
#define MAXATOMSTACK 256
#define MAXGROUPSTACK 256
#define MAXTABLE 256
#define MAXSEARCHSTACK 256
#define MAXLOOPTABLE 256
#define MAXCAPTURESTACK 256

// flags
#define NOFLAG 0 // regular character
#define EPSILON 1 // empty transition
#define ANY 2
#define CHARSET 3
#define CAPTUREOPEN 4
#define CAPTURECLOSE 5

// insertion priority
#define LOWPRIORITY 0
#define HIGHPRIORITY 1

//helper macros
#define checkBit(var, pos) ((var) & (1 << (pos)))
#define stackLen(sp, base) (int)(sp - base)

#define push(sp, val) \
    *sp = val;        \
    sp++

#define pop(sp) \
    *(sp - 1);  \
    sp--

#define peek(sp) (*(sp - 1))

typedef struct
{
    char *start;
    char *end;
    node *nodeStart;
} captureGroup;

typedef struct
{
    node *nodePos;
    char *charPos;

    captureGroup *captureMem;
    int memLength;

    char entryFlag;
} state;

typedef struct
{
    int capacity;
    int length;
    state *content;
} stateList;

typedef struct
{
    unsigned int length;
    bool capturing;
} group;

typedef struct
{
    node *start;
    node *end;
} atom;

typedef struct
{
    connection *connectionObject; // the given connection
    char *inputPosition;          // the position within the input string
} possibleConnection;

node *createNode()
{
    node *out = malloc(sizeof(node));

    out->connections = NULL;
    out->numConnections = 0;
    out->acceptingNode = false;

    return out;
}

void destroyNode(node *n)
{
    if (n->connections != NULL)
    {
        for (int i = 0; i < n->numConnections; i++)
        {
            free(n->connections[i].data);
        }
        free(n->connections);
    }

    free(n);
}

void pushNodeArray(nodeArray *list, node *item)
{
    if (list->capacity <= list->length)
    {
        list->capacity *= 2;
        list->content = realloc(list->content, list->capacity * sizeof(node *));
    }

    list->content[list->length] = item;
    list->length++;
}

// return index of node or -1
int inNodeArray(nodeArray *list, node *item)
{
    for (int i = 0; i < list->length; i++)
    {
        if (list->content[i] == item)
        {
            return i;
        }
    }

    return -1;
}

// return index of state or -1
int inStateList(stateList *list, state *item)
{
    for (int i = 0; i < list->length; i++)
    {
        if (list->content[i].charPos == item->charPos && list->content[i].nodePos == item->nodePos)
        {
            return i;
        }
    }

    return -1;
}

char *captureGroupToStr(captureGroup *group)
{

    size_t len = (size_t)(group->end - group->start);
    char *outStr = malloc(len + 1);
    outStr[len] = '\0';

    memcpy(outStr, group->start, len);

    return outStr;
}

captureGroup *copyCaptureStack(captureGroup *stackBase, captureGroup *stackPointer)
{
    int captureStackLen = (int)(stackPointer - stackBase);
    captureGroup *captureStackCopy = malloc(sizeof(captureGroup) * captureStackLen);
    memcpy(captureStackCopy, stackBase, sizeof(captureGroup) * captureStackLen);

    return captureStackCopy;
}

void pushStringList(stringList *list, char *item)
{
    if (list->capacity <= list->length)
    {
        list->capacity *= 2;
        list->content = realloc(list->content, list->capacity * sizeof(char *));
    }

    list->content[list->length] = item;
    list->length++;
}

void pushStateList(stateList *list, state *item)
{
    if (list->capacity <= list->length)
    {
        list->capacity *= 2;
        list->content = realloc(list->content, list->capacity * sizeof(state));
    }

    list->content[list->length] = *item;
    list->length++;
}

void freeStringList(stringList list)
{
    for (int i = 0; i < list.length; i++)
    {
        free(list.content[i]);
    }
    free(list.content);
}

connection *connectNodes(node *a, node *b, char accepted, unsigned char flag)
{
    a->numConnections++;
    a->connections = realloc(a->connections, a->numConnections * sizeof(connection));
    a->connections[a->numConnections - 1] = (connection){accepted, b, flag, NULL};

    if (flag == CHARSET)
    {
        a->connections[a->numConnections - 1].data = calloc(4, 8); // 32 bytes is 256 bits, sufficent to represent a bit array of ascii values
    }

    return &(a->connections[a->numConnections - 1]);
}

connection *priorityConnectNodes(node *a, node *b, char accepted, unsigned char flag, unsigned char priorityFlag)
{
    if (priorityFlag == HIGHPRIORITY)
    {
        return connectNodes(a, b, accepted, flag);
    }
    else if (priorityFlag == LOWPRIORITY)
    {
        a->numConnections++;

        connection *temp = malloc(a->numConnections * sizeof(connection));
        temp[0] = (connection){accepted, b, flag, NULL};
        if (a->numConnections > 1)
        {
            memcpy(temp + 1, a->connections, (a->numConnections - 1) * sizeof(connection));
        }
        free(a->connections);

        a->connections = temp;

        if (flag == CHARSET)
        {
            a->connections[0].data = calloc(4, 8); // 32 bytes is 256 bits, sufficent to represent a bit array of ascii values
        }

        return &(a->connections[0]);
    }

    return NULL;
}

regexReturn_t simulateNFA(node *entry, const char *input)
{
    // a stack holding all states to be searched
    state stateStack[MAXSEARCHSTACK];
    state *sp = stateStack;
    push(sp, ((state){entry, (char *)input, NULL, 0, EPSILON}));

    // a stack holding data for capture groups
    captureGroup captureStack[MAXCAPTURESTACK];
    captureGroup *captureStackPointer;

    // Captured output and captured start are related as each captured string of index i is captured starting at a node of the same index in the node list
    stringList capturedOutput = (stringList){1, 0, malloc(sizeof(char *))};
    nodeArray captureStart = (nodeArray){1, 0, malloc(sizeof(node *))};

    // we keep track of epsilon transitions to make sure we don't end up with infinite loops
    stateList epsilonTable = (stateList){MAXLOOPTABLE, 0, malloc(sizeof(state) * MAXLOOPTABLE)};

    // even once we have found and possible solution, because we are greedy we continue searching, thus we hold a found bool
    state current;
    bool found = false;
    while (sp != stateStack)
    {
        current = pop(sp);

        // here we copy our capture memory that was stored on the stack into our capture stack
        memcpy(captureStack, current.captureMem, sizeof(captureGroup) * current.memLength);
        free(current.captureMem); // we can then free the previously stored capture memory as it will never be accessed from that location again
        captureStackPointer = captureStack + current.memLength;

        if (current.entryFlag == CAPTURECLOSE)
        {
            captureGroup topCapture = pop(captureStackPointer);

            topCapture.end = current.charPos;

            char *outStr = captureGroupToStr(&topCapture);

            // we need to check if the capture group is merely being expanded as not to add a superflous entry to our output
            int duplicateIdx = inNodeArray(&captureStart, topCapture.nodeStart);
            if (duplicateIdx != -1)
            {
                free(capturedOutput.content[duplicateIdx]);
                capturedOutput.content[duplicateIdx] = outStr;
            }
            else
            {
                pushStringList(&capturedOutput, outStr);
                pushNodeArray(&captureStart, topCapture.nodeStart);
            }
        }

        if (current.nodePos->acceptingNode)
        {
            found = true;
            break; // this saves some computation as accepting node never have outward connections
        }

        for (int i = 0; i < current.nodePos->numConnections; i++)
        {
            connection conn = current.nodePos->connections[i];

            switch (conn.flag)
            {

            case EPSILON:
            {
                state newState = (state){conn.next, current.charPos};
                int duplicateIdx = inStateList(&epsilonTable, &newState); // check if our prospective state (if we were to follow the transition) is in the epsilon table

                if (duplicateIdx == -1)
                {
                    pushStateList(&epsilonTable, &newState);
                    // we have to go through this extra step of copying our capture stack before we store it on the stack
                    captureGroup *captureStackCopy = copyCaptureStack(captureStack, captureStackPointer);
                    push(sp, ((state){conn.next, current.charPos, captureStackCopy, stackLen(captureStackPointer, captureStack), EPSILON}));
                }
            }
            break;
            case ANY:
            {
                // we are dealing with c strings here so we need to add these sneaky bounds check
                if (*(current.charPos) == '\0')
                {
                    continue;
                }
                captureGroup *captureStackCopy = copyCaptureStack(captureStack, captureStackPointer);
                push(sp, ((state){conn.next, current.charPos + 1, captureStackCopy, stackLen(captureStackPointer, captureStack), ANY}));
            }
            break;
            case NOFLAG: // in retrospect NOFLAG is a bit confusing however it just means to treat the transition as a plain character
                if (*(current.charPos) == conn.acceptable)
                {
                    captureGroup *captureStackCopy = copyCaptureStack(captureStack, captureStackPointer);
                    push(sp, ((state){conn.next, current.charPos + 1, captureStackCopy, stackLen(captureStackPointer, captureStack), NOFLAG}));
                }
                break;
            case CHARSET:
            {
                // we do some bit magic here because chararcter sets are stored in bit arrays for memory compactness
                uint32_t seg32 = ((uint32_t *)conn.data)[*current.charPos / 32];
                bool bitVal = checkBit(seg32, *current.charPos % 32);

                if (bitVal)
                {
                    captureGroup *captureStackCopy = copyCaptureStack(captureStack, captureStackPointer);
                    push(sp, ((state){conn.next, current.charPos + 1, captureStackCopy, stackLen(captureStackPointer, captureStack), CHARSET}));
                }
            }
            break;
            case CAPTUREOPEN:
            {
                push(captureStackPointer, ((captureGroup){current.charPos, current.charPos, current.nodePos}));

                captureGroup *captureStackCopy = copyCaptureStack(captureStack, captureStackPointer);
                push(sp, ((state){conn.next, current.charPos, captureStackCopy, stackLen(captureStackPointer, captureStack), CAPTUREOPEN}));
            }
            break;
            case CAPTURECLOSE:
            {
                captureGroup *captureStackCopy = copyCaptureStack(captureStack, captureStackPointer);
                push(sp, ((state){conn.next, current.charPos, captureStackCopy, stackLen(captureStackPointer, captureStack), CAPTURECLOSE}));
            }
            break;
            default:
                printf("What? unknown connection flag\n");
                exit(-1);
                break;
            }
            if (sp >= stateStack + MAXSEARCHSTACK)
            {
                printf("OVERFLOW\n");
                exit(-1);
            }
        }
    }

    // clean ourselves up, no leaks around here
    free(epsilonTable.content);
    free(captureStart.content);

    if (!found)
    {
        freeStringList(capturedOutput);
        capturedOutput = (stringList){0, 0, NULL};
    }

    return (regexReturn_t){capturedOutput, current.charPos, found};
}


nodeArray *createNodeArray()
{
    nodeArray *out = malloc(sizeof(nodeArray));
    out->capacity = 16;

    out->content = malloc(sizeof(node *) * out->capacity);
    out->length = 0;

    return out;
}

node *emplaceNode(nodeArray *arr)
{

    if (arr->capacity <= arr->length)
    {
        arr->capacity *= 2;
        arr->content = realloc(arr->content, sizeof(node *) * arr->capacity);
    }
    arr->length++;
    node **last = &arr->content[arr->length - 1];

    *last = createNode();
    return *last;
}

void setBitVal(char ch, void *bitArr)
{
    uint32_t *seg32 = ((uint32_t *)bitArr) + (ch / 32);
    *seg32 |= (1 << (ch - ((ch / 32) * 32)));
}

void fillRange(char a, char b, void *bitArr)
{
    for (char i = a; i <= b; i++)
    {
        setBitVal(i, bitArr);
    }
}

atom createCharAtom(char character, unsigned char flag, nodeArray *nodeArr)
{
    node *start = emplaceNode(nodeArr);
    node *end = emplaceNode(nodeArr);
    char priority = (flag == EPSILON) ? LOWPRIORITY : HIGHPRIORITY;
    priorityConnectNodes(start, end, character, flag, priority);
    atom out = (atom){start, end};
    return out;
}

atom concatAtoms(atom *arr, int n, nodeArray *nodes)
{
    if (n == 0)
    {
        atom empty = createCharAtom('-', EPSILON, nodes);
        return empty;
    }
    else if (n == 1)
    {
        return *arr;
    }

    atom out;
    out.start = arr->start;
    for (int i = 0; i < n - 1; i++)
    {
        priorityConnectNodes(arr[i].end, arr[i + 1].start, '-', EPSILON, LOWPRIORITY);
    }

    out.end = arr[n - 1].end;
    return out;
}

atom createOr(atom *optionA, atom *optionB, nodeArray *nodeArr)
{
    atom out;

    out.start = emplaceNode(nodeArr);
    connectNodes(out.start, optionA->start, '-', EPSILON);
    connectNodes(out.start, optionB->start, '-', EPSILON);

    out.end = emplaceNode(nodeArr);
    connectNodes(optionA->end, out.end, '-', EPSILON);
    connectNodes(optionB->end, out.end, '-', EPSILON);

    return out;
}

atom copyAtom(atom *a, nodeArray *nodeArr)
{
    typedef struct
    {
        node *realNode;
        node *copiedNode;
    } nodeCopy;

    nodeCopy copyStack[MAXSEARCHSTACK];
    nodeCopy *copyStackPointer = copyStack;

    nodeCopy copyTable[MAXTABLE]; // this would be better as a hash map but I make the concession and keep it as a table
    int tableLen = 0;
    node *initialNode = emplaceNode(nodeArr);
    push(copyStackPointer, ((nodeCopy){a->start, initialNode}));

    atom out;
    out.start = initialNode;

    while (copyStackPointer != copyStack) // while there are still nodes to be processed
    {
        nodeCopy current = pop(copyStackPointer);

        if (current.realNode == a->end)
        {
            // don't traverse past the end, although it probably isn't even possible
            out.end = current.copiedNode;
            continue;
        }

        for (int i = 0; i < current.realNode->numConnections; i++)
        {
            bool found = false;
            connection currentConnection = current.realNode->connections[i];
            connection *newConn;
            for (int j = 0; j < tableLen; j++)
            {
                if (currentConnection.next == copyTable[j].realNode)
                {
                    newConn = connectNodes(current.copiedNode, copyTable[j].copiedNode, currentConnection.acceptable, currentConnection.flag);
                    found = true;
                    break;
                }
            }

            if (!found)
            {
                node *copied = emplaceNode(nodeArr);
                copied->acceptingNode = ((node *)currentConnection.next)->acceptingNode;
                newConn = connectNodes(current.copiedNode, copied, currentConnection.acceptable, currentConnection.flag);
                push(copyStackPointer, ((nodeCopy){currentConnection.next, copied}));

                copyTable[tableLen] = ((nodeCopy){currentConnection.next, copied});
                tableLen++;
            }

            if (currentConnection.flag == CHARSET) // Character sets must be copied
            {
                void *oldData = current.realNode->connections[current.realNode->numConnections - 1].data;
                memcpy(newConn->data, oldData, 32);
            }
        }
    }

    return out;
}

void negateBitArray(void *bitArr)
{
    for (int i = 0; i < 4; i++)
    {
        ((uint32_t *)bitArr)[i] = ~((uint32_t *)bitArr)[i];
    }
}

atom createSet(char **openingBracket, nodeArray *nodes)
{
    *openingBracket = *openingBracket + 1;

    atom setAtom = createCharAtom('?', CHARSET, nodes);
    void *bitArr = setAtom.start->connections->data; // the first and only connection
    bool negateSet = false;

    char *currentChar = *openingBracket;
    // we want to keep track of our index
    for (int i = 0; *currentChar != ']'; i++)
    {
        switch (*currentChar)
        {
        case '^':
            if (i == 0)
            {
                negateSet = true;
            }
            else
            {
                setBitVal(*currentChar, bitArr);
            }
            break;
        case '-':
            fillRange(*(currentChar - 1), *(currentChar + 1), bitArr);
            currentChar++;
            *openingBracket = *openingBracket + 1;
            break;
        case '\\':
            currentChar++;
            *openingBracket = *openingBracket + 1;
            setBitVal(*currentChar, bitArr);
            break;
        case '\0':
            printf("ERROR! unmatched '['\n");
            exit(-1);
            break;
        default:
            setBitVal(*currentChar, bitArr);
            break;
        }

        currentChar++;
        *openingBracket = *openingBracket + 1;
    }
    if (negateSet)
    {
        negateBitArray(bitArr);
    }

    return setAtom;
}

atom kleeneStarAtom(atom *a, nodeArray *nodes)
{
    atom empty = createCharAtom('-', EPSILON, nodes);

    priorityConnectNodes(empty.start, a->start, '-', EPSILON, HIGHPRIORITY);
    priorityConnectNodes(a->end, empty.end, '-', EPSILON, LOWPRIORITY);
    priorityConnectNodes(a->end, a->start, '-', EPSILON, HIGHPRIORITY);

    return empty;
}

void optionalAtom(atom *a)
{
    priorityConnectNodes(a->start, a->end, '-', EPSILON, LOWPRIORITY);
}

void plusAtom(atom *a)
{
    priorityConnectNodes(a->end, a->start, '-', EPSILON, HIGHPRIORITY);
}

atom processEscapeCharacter(char character, nodeArray *nodes)
{
    switch (character) // some escaped characters have special meaning, thus we must encode this meaning into our nfa
    {
    case 'd':
    {
        char *setExpression = "[0-9]";
        return createSet(&setExpression, nodes);
    }
    break;
    case 'w':
    {
        char *setExpression = "[a-zA-Z]";
        return createSet(&setExpression, nodes);
    }
    break;
    case 's':
    {
        char *setExpression = "[ \t\n\v\f\r]";
        return createSet(&setExpression, nodes);
    }
    break;
    case 'D':
    {
        printf("hello\n");
        char *setExpression = "[^0-9]";
        return createSet(&setExpression, nodes);
    }
    break;
    case 'W':
    {
        char *setExpression = "[^a-zA-Z]";
        return createSet(&setExpression, nodes);
    }
    break;
    case 'S':
    {
        char *setExpression = "[^ \t\n\v\f\r]";
        return createSet(&setExpression, nodes);
    }
    break;
    default:
        return createCharAtom(character, NOFLAG, nodes);
        break;
    }
}

int asNum(char **character)
{
    int out = 0;
    char digits[10];
    int length = 0;

    for (int i = 0; isdigit(**character) && i < 10; i++)
    {
        digits[i] = (**character);
        *character = *character + 1;
        length++;
    }

    for (int i = 0; i < length; i++)
    {
        out = 10 * out + (digits[i] - '0');
    }

    return out;
}

regex_t compileExpression(const char *expression)
{
    regex_t out;
    out.nodes = createNodeArray();

    // I am stealing some of the ideas from the shunting yard algorithm to do this, thus we implement an operator stack
    char operatorStack[MAXOPERATORSTACK];
    char *opStackPointer = operatorStack;

    atom atomStack[MAXATOMSTACK];
    atom *atomStackPointer = atomStack;

    // we count the number of characters within a group which is used for concatenation later, it is important for nesting and expressions like "(a(a|b)c)+"
    group groupStack[MAXGROUPSTACK];
    group *groupStackPointer = groupStack; // we start with a group holding 0 atoms
    push(groupStackPointer, ((group){0, true}));

    char *currentChar = (char *)expression;
    while (*currentChar != '\0')
    {
        switch (*currentChar)
        {
        case '|':
        {
            // pop group items from the atom stack and concatenate them
            group topGroup = pop(groupStackPointer);
            atomStackPointer -= topGroup.length;
            push(atomStackPointer, concatAtoms(atomStackPointer, groupStackPointer->length, out.nodes));

            push(groupStackPointer, ((group){0, topGroup.capturing})); // create new group
            push(opStackPointer, '|');
        }
        break;
        case '(':
        {
            bool capturing = true;
            //(?:) turns off the capturing group
            if (*(currentChar + 1) == '?')
            {
                if (*(currentChar + 2) == ':')
                {
                    capturing = false;
                    currentChar += 2;
                }
            }

            push(groupStackPointer, ((group){0, capturing}));
            push(opStackPointer, '(');
        }
        break;
        case ')':
        {
            // concatenate group
            group topGroup = pop(groupStackPointer);
            atomStackPointer -= topGroup.length;
            push(atomStackPointer, concatAtoms(atomStackPointer, groupStackPointer->length, out.nodes));

            // pop operatorStack until we hit a matching (, while this is being done we deal with creating or blocks
            do
            {
                if (operatorStack == opStackPointer)
                {
                    printf("ERROR! unmatched ')', %s\n", currentChar);
                    exit(-1);
                    break;
                }
                char currentOperator = pop(opStackPointer);
                if (currentOperator == '|')
                {
                    atomStackPointer -= 2;
                    push(atomStackPointer, createOr(atomStackPointer, atomStackPointer + 1, out.nodes));
                }
                else if (currentOperator == '(')
                {
                    if (topGroup.capturing)
                    {
                        node *start = emplaceNode(out.nodes);
                        node *end = emplaceNode(out.nodes);
                        atom *top = (atomStackPointer - 1);

                        connectNodes(start, top->start, '-', CAPTUREOPEN);
                        connectNodes(top->end, end, '-', CAPTURECLOSE);
                        top->start = start;
                        top->end = end;
                    }
                }
            } while (*opStackPointer != '(');
            peek(groupStackPointer).length++;
        }
        break;
        case '[':
        {
            atom setAtom = createSet(&currentChar, out.nodes); // we create an atom with a bit array based on the character set
            peek(groupStackPointer).length++;
            push(atomStackPointer, setAtom);
        }
        break;
        case '.':
            peek(groupStackPointer).length++;
            push(atomStackPointer, createCharAtom('-', ANY, out.nodes));
            break;
        case '\\': // backslash
            peek(groupStackPointer).length++;
            currentChar++;
            push(atomStackPointer, processEscapeCharacter(*currentChar, out.nodes));
            break;
        case '*':
        {
            if (atomStack == atomStackPointer)
            {
                printf("ERROR! improper use of '*'\n");
                exit(-1);
                break;
            }
            atom top = pop(atomStackPointer);
            push(atomStackPointer, kleeneStarAtom(&top, out.nodes));
        }
        break;
        case '?':
            if (atomStack == atomStackPointer)
            {
                printf("ERROR! improper use of '?'\n");
                exit(-1);
                break;
            }
            if (*(currentChar - 1) == '*')
            {
                node *newEnd = emplaceNode(out.nodes);
                priorityConnectNodes((atomStackPointer - 1)->start, newEnd, '-', EPSILON, HIGHPRIORITY);
                (atomStackPointer - 1)->end = newEnd;
            }
            else if (*(currentChar - 1) == '+')
            {
                node *newEnd = emplaceNode(out.nodes);
                priorityConnectNodes((atomStackPointer - 1)->end, newEnd, '-', EPSILON, HIGHPRIORITY);
                (atomStackPointer - 1)->end = newEnd;
            }
            else
            {
                optionalAtom(&peek(atomStackPointer));
            }
            break;
        case '+':
        {
            if (atomStack == atomStackPointer)
            {
                printf("ERROR! improper use of '+'\n");
                exit(-1);
                break;
            }
            atom top = pop(atomStackPointer);
            plusAtom(&top);
            push(atomStackPointer, top);
        }
        break;
        case '{':
        {
            currentChar++;
            int min = 0;
            int max = 0;

            atom *top = atomStackPointer - 1;
            if (isdigit(*currentChar))
            {
                min = asNum(&currentChar);

                for (int i = 0; i < min - 1; i++)
                {
                    push(atomStackPointer, copyAtom(top, out.nodes));
                }
            }

            if (*currentChar == '}')
            {
                max = min;
            }
            else if (*currentChar == ',')
            {
                currentChar++;
                if (isdigit(*currentChar))
                {
                    max = asNum(&currentChar);
                    min = (min == 0) ? 1 : min;
                    for (int i = 0; i < max - min; i++)
                    {
                        atom copy = copyAtom(top, out.nodes);
                        optionalAtom(&copy);
                        push(atomStackPointer, copy);
                    }

                    if (min == 0)
                    {
                        optionalAtom(top);
                    }
                }
                else if (*currentChar == '}')
                {
                    atom copy;
                    if (min == 0)
                    {
                        copy = *top;
                        max = 1;
                    }
                    else
                    {
                        copy = copyAtom(top, out.nodes);
                        max = min;
                    }

                    push(atomStackPointer, kleeneStarAtom(&copy, out.nodes));
                }
            }

            atomStackPointer -= max;
            push(atomStackPointer, concatAtoms(atomStackPointer, max, out.nodes));

            node *temp = emplaceNode(out.nodes);
            priorityConnectNodes(peek(atomStackPointer).end, temp, '-', EPSILON, LOWPRIORITY);
            peek(atomStackPointer).end = temp;
        }
        break;
        default:
            // create two nodes and connect them by the current character, push this atom to the stack
            peek(groupStackPointer).length++;
            push(atomStackPointer, createCharAtom(*currentChar, NOFLAG, out.nodes));
            break;
        }
        currentChar++;
    }
    // concat remaining
    if (groupStack != groupStackPointer)
    {
        group topGroup = pop(groupStackPointer);
        atomStackPointer -= topGroup.length;
        push(atomStackPointer, concatAtoms(atomStackPointer, groupStackPointer->length, out.nodes));
    }

    // loop operator stack
    while (operatorStack != opStackPointer)
    {
        char currentOperator = pop(opStackPointer);
        if (currentOperator == '|')
        {
            atomStackPointer -= 2;
            push(atomStackPointer, createOr(atomStackPointer, atomStackPointer + 1, out.nodes));
        }
        else
        {
            printf("ERROR! symbol: '%c'\n", currentOperator);
            exit(-1);
        }
    }

    int length = (atomStackPointer - atomStack); // find the number of elements on the atomStack
    atom groupedOutput = concatAtoms(atomStack, length, out.nodes);

    // concat an accepting node onto the end of the nfa
    out.startNode = emplaceNode(out.nodes);
    priorityConnectNodes(out.startNode, groupedOutput.start, '-', CAPTUREOPEN, HIGHPRIORITY);

    node *final = emplaceNode(out.nodes);
    priorityConnectNodes(groupedOutput.end, final, '-', CAPTURECLOSE, LOWPRIORITY);
    final->acceptingNode = true;

    return out;
}

void freeRegex(regex_t *expr)
{
    for (int i = 0; i < expr->nodes->length; i++)
    {
        destroyNode(expr->nodes->content[i]);
    }

    free(expr->nodes->content);
    free(expr->nodes);
}

regexReturn_t findFirst(regex_t *expression, const char *input)
{
    return simulateNFA(expression->startNode, input);
}

regexReturn_t findAll(regex_t *expression, const char *input)
{
    stringList allStrings = {1, 0, malloc(sizeof(char *))};
    char* last = (char*) input;

    while(*last != '\0')
    {
        regexReturn_t ret = simulateNFA(expression->startNode, last);
        for(int i = 0; i < ret.capturedStrings.length; i++)
        {
            pushStringList(&allStrings, ret.capturedStrings.content[i]);
        }
        free(ret.capturedStrings.content);
      
        last = (ret.success) ? ret.readEnd : ret.readEnd+1;
    }

    return (regexReturn_t){allStrings, last, (allStrings.length > 0)};
}

#endif