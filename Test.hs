module Test where

import Test.HUnit
import Solucion

run = runTestTT tests

tests = test [
    "nombreDeUsuario con repetidos"   ~: (nombreDeUsuario red1)             ~?= ["Juan", "Sofia", "Jazmin", "Nico", "Marco"],
    "nombreDeUsuario sin repetidos"   ~: (nombreDeUsuario red2)             ~?= ["Juan", "Sofia", "Jazmin", "Nico", "Marco"],
    "nombreDeUsuario vacio"           ~: (nombreDeUsuario red0)             ~?= [],

    "amigosDe sin amigos"             ~: (amigosDe red1 u7)                 ~?= [],
    "amigosDe con amigos"             ~: (amigosDe red1 u1)                 ~?= [u2, u3, u4, u5, u6],

    "cantAmigos sin amigos"           ~: (cantidadDeAmigos red1 u7)         ~?= 0,
    "cantAmigos con amigos"           ~: (cantidadDeAmigos red1 u1)         ~?= 5,

    "Usuario con mas amigos mas de 1" ~: (usuarioConMasAmigos red3)         ~?= u1,
    "Usuario con mas amigos uno solo" ~: (usuarioConMasAmigos red1)         ~?= u1,

    "Esta RobertoCarlos"              ~: (estaRobertoCarlos red1)           ~?= True,
    "No esta Roberto Carlos"             ~: (estaRobertoCarlos red2)           ~?= False,
    "No esta nadie"                      ~: (estaRobertoCarlos red0)           ~?= False,

    "PublicacionesDe hay publicacion"    ~: (publicacionesDe red1 u1)          ~?= [p1_1, p1_2, p1_3]
    "PublicacionesDe no hay"             ~: (publicacionesDe red1 u3)          ~?= [],

    "Hay publicaciones q le gustan"      ~: (publicacionesQueLeGustan red1 u2) ~?= [p1_1, p1_3],
    "No hay publicaciones que le gustan" ~: (publicacionesQueLeGustan red1 u6) ~?= [],

    "Le gustan las mismas publis"            ~: (lesGustanMismasPublicaciones red1 u1 u3) ~?= True,
    "No le gustan las mismas publis"         ~: (lesGustanMismasPublicaciones red2 u1 u3) ~?= False,

    "Tiene un seguidor Fiel"                 ~: (tieneUnSeguidorFiel red1 u1)             ~?= True,
    "Es su propio seguidor Fiel"             ~: (tieneUnSeguidorFiel red2 u1)             ~?= False,
    "No tiene seguidor Fiel"                 ~: (tieneUnSeguidorFiel red3 u1)             ~?= False,

--    "Existe Secuencia De amigos"             ~:
]