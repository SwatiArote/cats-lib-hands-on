val a = Set(1,6,5,4)
val b = Set(2,3,4,1)

a.intersect(b)
b.intersect(a)

(a.intersect(b)).union(b.intersect(a))