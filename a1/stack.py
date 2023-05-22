from collections import deque


class Stack(deque):
    def push(self, __x) -> None:
        self.append(__x)

    def peek(self):
        return self[len(self) - 1]
