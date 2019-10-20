class Garden(object):
    DEFAULT_STUDENTS = [
        "Alice",
        "Bob",
        "Charlie",
        "David",
        "Eve",
        "Fred",
        "Ginny",
        "Harriet",
        "Ileana",
        "Joseph",
        "Kincaid",
        "Larry",
    ]

    PLANTS = {
        "C": "Clover",
        "G": "Grass",
        "R": "Radishes",
        "V": "Violets",
    }

    def __init__(self, diagram, students=DEFAULT_STUDENTS):
        self._students = sorted(students)
        self._diagram = diagram.splitlines()
        if (
            len(self._diagram) != 2
            or len(self._diagram[0]) != len(self._diagram[1])
            or len(self._diagram[0]) % 2 != 0
        ):
            raise ValueError("Diagram should have two even, equal lines")

    def plants(self, student):
        pos = self._students.index(student) * 2
        return [
            self.PLANTS[plant]
            for plant in self._diagram[0][pos:pos + 2] + self._diagram[1][pos:pos + 2]
        ]
