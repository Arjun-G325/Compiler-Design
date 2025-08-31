struct Point {
    int x;
    int y;
};

int distance(struct Point *p1, struct Point *p2) {
    int dx = p1->x - p2->x;
    int dy = p1->y - p2->y;
    return dx*dx + dy*dy;
}

int main() {
    struct Point pts[2] = {{0, 0}, {3, 4}};
    return distance(&pts[0], &pts[1]);
}
