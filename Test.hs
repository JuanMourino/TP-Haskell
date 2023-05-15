module Test where
import Test.HUnit
import Solucion

main = runTestTT tests

tests = test [
    " nombresDeUsuarios 1" ~: (nombresDeUsuarios red1) ~?= ["Juan","Sofia","Nico","Marco","Jazmin"],
    
 ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Ejemplos

-- Usuarios:
u1 = (1, "Juan")
u2 = (2, "Sofia")
u3 = (3, "Juan")
u4 = (4, "Nico")
u5 = (5, "Marco")
u6 = (6, "Jazmin")
u7 = (7, "Sin Amigos")


-- Relaciones:
r1 = (u1, u2)
r2 = (u3, u1)
r3 = (u1, u4)
r4 = (u2, u3)
r5 = (u1, u5) --Cambiando Roberto Carlos por 5 amigos, da diferente con o sin r6
r6 = (u1, u6)

-- Publicaciones:
p1 = (u1, "1ra publicacion", [u1, u2, u3, u4])
p2 = (u1, "2da publicacion", [u1, u3, u2])
p3 = (u1, "3ra publicacion", [u1, u3])
pf = (u1, "Falso positivo", [u1])

-- Redes:

red0 = ([],[],[])

red1 = ([u1, u2, u3, u4, u5, u6], [r1, r2, r3, r4, r5, r6], [p1, p2, p3])
--Con red1 deberia dar True Roberto Carlos con 5 amigos, seguidor fiel para u1 y le gustan mismas publicaciones con (u1, u3)

red2 = ([u1, u2, u3, u4, u5], [r1, r2, r3, r4, r5], [p1, p2, pf])
--Con red2 deberia dar False Roberto Carlos, seguidor fiel para u1 y le gustan mismas publicaciones con (u1, u3)
--Deberia dar True para le gustan mismas publicaciones con (u2, u3)

