module Test where

import Test.HUnit
import Solucion

run = runTestTT tests

tests = test [
    "nombreDeUsuario con repetidos"   ~: (nombresDeUsuarios red1)             ~?= ["Juan", "Sofia", "Jazmin", "Nico", "Marco", "Sin amigos"],
    "nombreDeUsuario sin repetidos"   ~: (nombresDeUsuarios red2)             ~?= ["Juan", "Sofia", "Jazmin", "Nico", "Marco"],
    "nombreDeUsuario vacio"           ~: (nombresDeUsuarios red0)             ~?= [],

    "amigosDe sin amigos"             ~: (amigosDe red1 u7)                 ~?= [],
    "amigosDe con amigos"             ~: (amigosDe red1 u1)                 ~?= [u2, u3, u4, u5, u6],

    "cantAmigos sin amigos"           ~: (cantidadDeAmigos red1 u7)                          ~?= 0,
    "cantAmigos con amigos"           ~: (cantidadDeAmigos red1 u1)                          ~?= 5,

    "Usuario con mas amigos mas de 1"        ~: expectAny (usuarioConMasAmigos red3)          [u1, u2, u3],
    "Usuario con mas amigos uno solo"        ~: (usuarioConMasAmigos red1)                   ~?= u1,

    "Esta RobertoCarlos"                     ~: (estaRobertoCarlos red1)                     ~?= True,
    "No esta Roberto Carlos"                 ~: (estaRobertoCarlos red2)                     ~?= False,
    "No esta nadie"                          ~: (estaRobertoCarlos red0)                     ~?= False,

    "PublicacionesDe hay publicacion"        ~: (publicacionesDe red1 u1)                    ~?= [p1d1, p1d2, p1d3],
    "PublicacionesDe no hay"                 ~: (publicacionesDe red1 u3)                    ~?= [],

    "Hay publicaciones q le gustan"          ~: (publicacionesQueLeGustanA red1 u2)          ~?= [p1d1, p1d3],
    "No hay publicaciones que le gustan"     ~: (publicacionesQueLeGustanA red1 u6)          ~?= [],

    "Le gustan las mismas publis"            ~: (lesGustanLasMismasPublicaciones red1 u1 u3) ~?= True,
    "No le gustan las mismas publis"         ~: (lesGustanLasMismasPublicaciones red2 u1 u3) ~?= False,

    "Tiene un seguidor Fiel"                 ~: (tieneUnSeguidorFiel red1 u1)                ~?= True,
    "Es su propio seguidor Fiel"             ~: (tieneUnSeguidorFiel red2 u1)                ~?= False,
    "No tiene seguidor Fiel"                 ~: (tieneUnSeguidorFiel red3 u1)                ~?= False,

    "Existe Secuencia De amigos"             ~: (existeSecuenciaDeAmigos red4 u1 u5)         ~?= True,
    "No existe Secuencia De amigos"          ~: (existeSecuenciaDeAmigos red5 u1 u5)         ~?= False]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)
--Usuarios:
u1 = (1, "Juan")
u2 = (2, "Sofia")
u3 = (3, "Jazmin")
u4 = (4, "Nico")
u5 = (5, "Marco")
u6 = (6, "Juan")
u7 = (7, "Sin amigos")


--Relaciones:
r1 = (u1, u2)
r2 = (u3, u1)
r3 = (u1, u4)
r4 = (u2, u3)
r5 = (u1, u5)
r6 = (u1, u6) --Cambiando Roberto Carlos por 5 amigos, da diferente con o sin r6
r7 = (u3, u2)
r8 = (u3, u4)
r9 = (u1, u6)
r10 = (u5, u4)

--Publicaciones:
p1d1 = (u1, "1ra publicacion", [u1, u2, u3, u4])
p1d2 = (u1, "2da publicacion", [u1, u3])
p1d3 = (u1, "3ra publicacion", [u1, u3, u2])
p1df = (u1, "Falso positivo", [u1])
p1d4 = (u1, "Sin seguidor fiel", [])
p2d1 = (u2, "1ra pub2",  [])

--Redes:
red0 = ([], [], [])
red1 = ([u1, u2, u3, u4, u5, u6, u7], [r1, r2, r3, r4, r5, r6], [p1d1, p1d2, p1d3])
--Deberia dar True Roberto Carlos con 5 amigos, seguidor fiel para u1 y le gustan mismas publicaciones con (u1, u3)
red2 = ([u1, u2, u3, u4, u5], [r1, r2, r3, r4, r5], [p1d1, p1d2, p1df])
--Deberia dar False Roberto Carlos, seguidor fiel para u1 y le gustan mismas publicaciones con (u1, u3)
--Deberia dar True para le gustan mismas publicaciones con (u2, u3)
red3 = ([u1, u2 ,u3 ,u4], [r1, r2, r4],[p1d4])
red4 = ([u1, u2, u3, u4, u5, u6, u7], [r10, r1, r8, r7, r9], [])
red5 = ([u1, u2, u3, u4, u5, u6, u7], [r10, r1, r7, r9], [])