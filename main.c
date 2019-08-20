#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

// forward declarations
typedef struct _arc arc;
typedef struct _node node;

#define lmin(x, y) (((x) < (y)) ? (x) : (y))

struct _arc
{
    // source node index
    long from;
    // dest node index
    long to;

    long lowerBound;
    // used as upper bound and as residual capacity
    long upperBound;
    long cost;

    // current flow
    long flow;
};

inline void initArc(arc* a) {
    a->from = -1;
    a->to = -1;
    a->lowerBound = 0;
    a->upperBound = 0;
    a->cost = 0;
    a->flow = 0;
}

struct _node
{
    // list of outgoing arcs index
    long* outArcs;
    // list of incoming arcs (same as outgoing residual arcs)
    long* inArcs;
    // number of outgoing arcs
    long outArcsCount;
    // number of incoming arcs (same as outgoing residual arcs)
    long inArcsCount;
    // node capacity
    long capacity;
};

inline void initNode(node* n) {
    n->outArcs = NULL;
    n->inArcs = NULL;
    n->outArcsCount = 0;
    n->inArcsCount = 0;
    n->capacity = 0;
}

int readProblem(char* fileName, node** _nodes, arc** _arcs, long* _nodesNum, long* _arcsNum)
{
    FILE* inFile = fopen(fileName, "r");
    if (inFile == NULL) {
        printf("Can not open %s\n", fileName);
        return -1;
    }

    // arrays
    node* nodes = NULL;
    arc* arcs = NULL;
    long nodesNum = 0;
    long arcsNum = 0;

    long lineNum = 0;
    long arcNum = 0;

    char buffer[512];
    while (fgets(buffer, sizeof buffer, inFile) != NULL) {
        if (buffer[0] == 'c') {
            // comment, skip
            continue;
        } else if (buffer[0] == 'p') {
            // problem descriptor
            if (nodesNum != 0 || arcsNum != 0) {
                printf("Duplicate p descriptor on line %ld\n", lineNum);
                return -1;
            }

            sscanf(buffer, "p min %ld %ld\n", &nodesNum, &arcsNum);

            // add source node and destination node
            nodesNum += 2;

            nodes = malloc(sizeof(node) * nodesNum);
            for (long i = 0; i < nodesNum; i++) {
                initNode(&nodes[i]);
            }

            arcs = malloc(sizeof(arc) * arcsNum);
            for (int i = 0; i < arcsNum; i++) {
                initArc(&arcs[i]);
            }

            continue;
        } else if (buffer[0] == 'n') {
            // node descriptor
            long id = -1;
            long flow = 0;

            sscanf(buffer, "n %ld %ld", &id, &flow);

            if (id >= nodesNum) {
                printf("Node id out of bounds on line %ld\n", lineNum);
                return -1;
            }

            nodes[id].capacity = flow;

            continue;
        } else if (buffer[0] == 'a') {
            // arc descriptor

            long from = 0;
            long to = 0;
            long lowerBound = 0;
            long upperBound = 0;
            long cost = 0;

            sscanf(buffer, "a %ld %ld %ld %ld %ld", &from, &to, &lowerBound, &upperBound, &cost);
            if ((from > nodesNum - 2) || (to > nodesNum - 2) || arcNum > arcsNum) {
                printf("Wrong arc input on line %ld\n", lineNum);
                return -1;
            }

            arcs[arcNum].from = from;
            arcs[arcNum].to = to;
            arcs[arcNum].lowerBound = lowerBound;
            arcs[arcNum].upperBound = upperBound;
            arcs[arcNum].cost = cost;

            nodes[from].outArcsCount++;
            nodes[to].inArcsCount++;

            arcNum++;
            continue;
        } else {
            printf("Wrong descriptor on line %ld\n", lineNum);
            return -1;
        }
    }

    *_nodes = nodes;
    *_arcs = arcs;
    *_nodesNum = nodesNum;
    *_arcsNum = arcsNum;

    return 0;
}

int normalizeProblem(node** _nodes, arc** _arcs, long* _nodesNum, long* _arcsNum, long* _delta, long** _deltas)
{
    // arrays
    node* nodes = *_nodes;
    arc* arcs = *_arcs;
    long nodesNum = *_nodesNum;
    long arcsNum = *_arcsNum;
    long delta;
    long* deltas = *_deltas;

    // 1) remove lower bounds
    delta = 0;
    deltas = malloc(arcsNum * sizeof(long));
    for (int i = 0; i < arcsNum; i++) {
        deltas[i] = 0;
    }

    for (int i = 0; i < arcsNum; i++) {
        if (arcs[i].lowerBound > 0) {
            nodes[arcs[i].from].capacity -= arcs[i].lowerBound;
            nodes[arcs[i].to].capacity += arcs[i].lowerBound;
            arcs[i].upperBound -= arcs[i].lowerBound;
            deltas[i] = arcs[i].lowerBound;
            delta += arcs[i].lowerBound * arcs[i].cost;
            arcs[i].lowerBound = 0;
        }
    }

    // 2) remove multiple sources and destinations

    // calculate how much arcs we need to add
    long deltaArcs = 0;
    for (int i = 1; i < nodesNum - 1; i++) {
        if (nodes[i].capacity > 0) {
            // source node
            deltaArcs++;
            nodes[0].outArcsCount++;
            nodes[i].inArcsCount++;
        }
        if (nodes[i].capacity < 0) {
            // dest node
            deltaArcs++;
            nodes[i].outArcsCount++;
            nodes[nodesNum - 1].inArcsCount++;
        }
    }
    // resize arcs array
    arcs = (arc*)realloc(arcs, (arcsNum+deltaArcs)*sizeof(arc));
    for (int i = arcsNum; i < arcsNum+deltaArcs; i++) {
        //arcs[i] = malloc(sizeof(arc));
        initArc(&arcs[i]);
    }
    // process new arcs
    long arcNum = arcsNum;
    // skip 0 and last node - they are virtual source and dest
    for (int i = 1; i < nodesNum - 1; i++) {
        if (nodes[i].capacity > 0) {
            // source node
            arcs[arcNum].from = 0;
            arcs[arcNum].to = i;
            arcs[arcNum].lowerBound = 0;
            arcs[arcNum].upperBound = labs(nodes[i].capacity);
            arcs[arcNum].cost = 0;

            nodes[0].capacity += nodes[i].capacity;
            nodes[i].capacity = 0;

            arcNum++;
            continue;
        }
        if (nodes[i].capacity < 0) {
            // dest node
            arcs[arcNum].from = i;
            arcs[arcNum].to = nodesNum - 1;
            arcs[arcNum].lowerBound = 0;
            arcs[arcNum].upperBound = labs(nodes[i].capacity);
            arcs[arcNum].cost = 0;

            nodes[nodesNum  - 1].capacity += nodes[i].capacity;
            nodes[i].capacity = 0;

            arcNum++;
            continue;
        }
    }
    arcsNum += deltaArcs;

    // 3) store arc indexes in nodes
    long* nodesOutArc = malloc(sizeof(long) * nodesNum);
    long* nodesInArc = malloc(sizeof(long) * nodesNum);
    for (int i = 0; i < nodesNum; i++) {
        nodes[i].outArcs = malloc(sizeof(long) * nodes[i].outArcsCount);
        nodesOutArc[i] = 0;
        nodes[i].inArcs = malloc(sizeof(long) * nodes[i].inArcsCount);
        nodesInArc[i] = 0;
    }
    for (int i = 0; i < arcsNum; i++) {
        nodes[arcs[i].from].outArcs[nodesOutArc[arcs[i].from]] = i;
        nodesOutArc[arcs[i].from]++;
        nodes[arcs[i].to].inArcs[nodesInArc[arcs[i].to]] = i;
        nodesInArc[arcs[i].to]++;
    }

    *_nodes = nodes;
    *_arcs = arcs;
    *_nodesNum = nodesNum;
    *_arcsNum = arcsNum;
    *_delta = delta;
    *_deltas = deltas;

    return 0;
}

int buildResidualArcs(node* nodes, arc* arcs, arc** _residualArcs, long nodeCount, long arcCount)
{
    // arrays
    arc* residualArcs = malloc(arcCount * sizeof(arc));

    for (int i = 0; i < arcCount; i++) {
        initArc(&residualArcs[i]);
        residualArcs[i].from = arcs[i].to;
        residualArcs[i].to = arcs[i].from;
        residualArcs[i].lowerBound = 0;
        residualArcs[i].upperBound = 0;
        residualArcs[i].cost = -arcs[i].cost;
    }

    *_residualArcs = residualArcs;
    return 0;
}

int solveProblem(node* nodes, arc* arcs, long nodeCount, long arcCount)
{
    arc* residualArcs = NULL;

    // 1) build residual network
    if (buildResidualArcs(nodes, arcs, &residualArcs, nodeCount, arcCount)) {
        printf("Could not build residual arcs\n");
        return -1;
    }

    // if we still can find better solution - proceed
    bool interrupt = false;
    while (!interrupt) {
        // find shortest path from first to last node (considuring only costs of arcs)

        // shortest path. We store not previous node but arc wich leads to previous node. Negative values means residual arcs
        // 0 does not have e negative velue, so we just simply +1 to arc indexes
        long* path = malloc(nodeCount * sizeof(long));
        for (int i = 0; i < nodeCount; i++) {
            path[i] = -1;
        }
        // shortest path lengths
        long* length = malloc(nodeCount * sizeof(long));
        length[0] = 0;
        for (int i = 1; i < nodeCount; i++) {
            length[i] = LONG_MAX;
        }
        long* minResidualCap = malloc(nodeCount * sizeof(long));
        for (int i = 0; i < nodeCount; i++) {
            minResidualCap[i] = LONG_MAX;
        }

        // Bellman-Ford
        bool relaxed = false;
        for (int i = 0; i < nodeCount-1; i++) {
            relaxed = false;
            for (int j = 0; j < arcCount; j++) {
                if (length[arcs[j].from] == LONG_MAX) {
                    // skip
                    continue;
                }
                // check if we can use arc (residual capacity > 0)
                if (arcs[j].upperBound > 0) {
                    if (length[arcs[j].from] + arcs[j].cost < length[arcs[j].to] ) {
                        // shortest path relax
                        length[arcs[j].to] = length[arcs[j].from] + arcs[j].cost;
                        path[arcs[j].to] = j+1;
                        minResidualCap[arcs[j].to] = lmin(minResidualCap[arcs[j].from], arcs[j].upperBound);
                        relaxed = true;
                    }
                }
                // check if we can use residual arc (residual capacity > 0)
                if (residualArcs[j].upperBound > 0) {
                    if (length[residualArcs[j].from] + residualArcs[j].cost < length[residualArcs[j].to] ) {
                        // shortest path relax
                        length[residualArcs[j].to] = length[residualArcs[j].from] + residualArcs[j].cost;
                        path[residualArcs[j].to] = -(j+1);
                        minResidualCap[residualArcs[j].to] = lmin(minResidualCap[residualArcs[j].from], residualArcs[j].upperBound);
                        relaxed = true;
                    }
                }
            }
            if (!relaxed) {
                break;
            }
        }

        // if we have not found any path - exit
        if (length[nodeCount-1] == LONG_MAX) {
            interrupt = true;
            continue;
        }

        // if we have found some shortest path - get minimum residual capacity from it and add it to flow
        // start from the end
        long idx = nodeCount - 1;
        while (idx != 0) {
            // get arc leading to this node
            long arcIdx = path[idx];
            if (arcIdx > 0) {
                // normal flow. Increase normal flow
                arcIdx--;
                arcs[arcIdx].flow += minResidualCap[nodeCount-1];
                arcs[arcIdx].upperBound -= minResidualCap[nodeCount-1];
                residualArcs[arcIdx].flow -= minResidualCap[nodeCount-1];
                residualArcs[arcIdx].upperBound += minResidualCap[nodeCount-1];
                idx = arcs[arcIdx].from;
            }
            if (arcIdx < 0) {
                // reverse flow. Decrease normal flow to save cost
                arcIdx*=-1;
                arcIdx--;
                arcs[arcIdx].flow -= minResidualCap[nodeCount-1];
                arcs[arcIdx].upperBound += minResidualCap[nodeCount-1];
                residualArcs[arcIdx].flow += minResidualCap[nodeCount-1];
                residualArcs[arcIdx].upperBound -= minResidualCap[nodeCount-1];
                idx = arcs[arcIdx].from;
            }
        }

    }

    return 0;
}

int unnormolizeProblem(node* nodes, arc* arcs, long nodeCount, long arcCount, long* deltas)
{
    for (int i = 0; i < arcCount; i++) {
        arcs[i].flow += deltas[i];
    }

    return 0;
}

int printSolution(node* nodes, arc* arcs, long nodeCount, long arcCount)
{
    long long sol = 0;
    for (int i = 0; i < arcCount; i++) {
        sol += arcs[i].flow * arcs[i].cost;
        printf("a %ld %ld %ld\n", arcs[i].from, arcs[i].to, arcs[i].flow);
    }

    printf("%lld\n", sol);

    return 0;
}

int main(int argc, char** argv)
{
    if (argc < 2) {
        printf("No input file specifyed\n");
        return -1;
    }

    if (argc > 2) {
        printf("Wrong number of parameters\n");
        return -1;
    }

    // arrays
    node* nodes = NULL;
    arc* arcs = NULL;

    long nodesCount = 0;
    long arcsCount = 0;


    if (readProblem(argv[1], &nodes, &arcs, &nodesCount, &arcsCount)) {
        printf("Could not read input file\n");
        return -1;
    }

    long originalArcsCount = arcsCount;
    long delta = 0;
    long* deltas = NULL;

    if (normalizeProblem(&nodes, &arcs, &nodesCount, &arcsCount, &delta, &deltas)) {
        printf("Could not normolize problem\n");
        return -1;
    }

    if (solveProblem(nodes, arcs, nodesCount, arcsCount)) {
        printf("Could not solve problem\n");
        return -1;
    }

    if (unnormolizeProblem(nodes, arcs, nodesCount, originalArcsCount, deltas)) {
        printf("Could not unnormolize problem\n");
        return -1;
    }

    if (printSolution(nodes, arcs, nodesCount, originalArcsCount)) {
        printf("Could not print solution\n");
        return -1;
    }

    return 0;
}
