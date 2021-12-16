import numpy as np


class BlankClass():
    def __init__(self):
        pass

class Person():
    def __init__(self, forename, surname):
        # Do some setup stuff
        # Like setting attributes based on variables
        self.forename = forename
        self.surname = surname

    def __str__(self):
        # Defines what str() should do to the class
        return self.forename + " " + self.surname

    def sayName(self):
        # print() actually calls str()
        # So should just print the name
        print(self)

class Programmer(Person):
    # Programmer class inherits from Person class
    def __init__(self, forename, surname, languagesKnown):
        # Init for class
        self._languages = languagesKnown


        # Call init of Person explicitly
        Person.__init__(self, forename, surname)



me = Programmer("Tom", "Rocke", ["Python", "Fortran", "C#", "JavaScript"])
me.sayName()

blank = BlankClass()
print(blank)