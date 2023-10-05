| package |
package := Package name: 'TP'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Abogado;
	add: #AbogadoCobroFijo;
	add: #AbogadoCobroPje;
	add: #Causa;
	add: #Estudio;
	add: #Fuero;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'Core\Object Arts\Dolphin\Base\Dolphin'
	'Core\Object Arts\Dolphin\Base\Dolphin Legacy Date & Time'
	'Core\Object Arts\Dolphin\Base\Dolphin Message Box'
	'Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter'
	'UTN').

package!

"Class Definitions"!

Object subclass: #Causa
	instanceVariableNames: 'nroCausa demandado fecha_ini fuero_procesal juzgado_radicacion sentencia motivos monto fecha_fin estado demandante abogado'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Estudio
	instanceVariableNames: 'abogados causas listadoFueros'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Fuero
	instanceVariableNames: 'nombre descripcion'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Persona subclass: #Abogado
	instanceVariableNames: 'cantCausasAbiertas causasAbiertas codigo montoFijoXCausa porcentajeXCausas cantFalloFavorables fueroProcesal'
	classVariableNames: 'UltimoCodigo'
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Abogado subclass: #AbogadoCobroFijo
	instanceVariableNames: 'monto'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Abogado subclass: #AbogadoCobroPje
	instanceVariableNames: 'porcentaje'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Causa guid: (GUID fromString: '{77a76888-9d27-4f88-9b18-a59d68198bb0}')!
Causa comment: ''!
!Causa categoriesForClass!Kernel-Objects! !
!Causa methodsFor!

abogado
	^abogado!

abogado: anObject
	abogado := anObject!

abrirCausa
self nroCausa:(Prompter prompt: 'Ingrese el numero de la causa').

!

cerrarCausa
fecha_fin := Date today.
estado := 'finalizado'.
sentencia := Prompter prompt: 'Ingrese la sentencia'.
[sentencia = 'favorable' or: [sentencia = 'no favorable'].] whileFalse: [sentencia := Prompter prompt: 'Ingrese la sentencia'.].
!

demandado
	^demandado!

demandado: anObject
	demandado := anObject!

demandante
	^demandante!

demandante: anObject
	demandante := anObject!

estado
	^estado!

estado: anObject
	estado := anObject!

fecha_fin
	^fecha_fin!

fecha_fin: anObject
	fecha_fin := anObject!

fecha_ini
	^fecha_ini!

fecha_ini: anObject
	fecha_ini := anObject!

fuero_procesal
	^fuero_procesal!

fuero_procesal: anObject
	fuero_procesal := anObject!

juzgado_radicacion
	^juzgado_radicacion!

juzgado_radicacion: anObject
	juzgado_radicacion := anObject!

monto
	^monto!

monto: anObject
	monto := anObject!

motivos
	^motivos!

motivos: anObject
	motivos := anObject!

nro_causa
	^nroCausa!

nro_causa: unNro
nroCausa := unNro!

sentencia
	^sentencia!

sentencia: anObject
	sentencia := anObject! !
!Causa categoriesForMethods!
abogado!accessing!private! !
abogado:!accessing!private! !
abrirCausa!public! !
cerrarCausa!public! !
demandado!accessing!private! !
demandado:!accessing!private! !
demandante!accessing!private! !
demandante:!accessing!private! !
estado!accessing!private! !
estado:!accessing!private! !
fecha_fin!accessing!private! !
fecha_fin:!accessing!private! !
fecha_ini!accessing!private! !
fecha_ini:!accessing!private! !
fuero_procesal!accessing!private! !
fuero_procesal:!accessing!private! !
juzgado_radicacion!accessing!private! !
juzgado_radicacion:!accessing!private! !
monto!accessing!private! !
monto:!accessing!private! !
motivos!accessing!private! !
motivos:!accessing!private! !
nro_causa!accessing!public! !
nro_causa:!public! !
sentencia!accessing!private! !
sentencia:!accessing!private! !
!

Estudio guid: (GUID fromString: '{6daa5c0e-e76c-4431-a637-708c15fa5c69}')!
Estudio comment: ''!
!Estudio categoriesForClass!Kernel-Objects! !
!Estudio methodsFor!

altaAbogado
|abogado b fuero|
b:= 0.
abogado := Abogado new.
abogado cargaDatos.
[b = 1.] whileFalse: [fuero := Prompter prompt: 'Ingrese el fuero del abogado'.
[fuero='*'] ifTrue:[b=0]
ifFalse:[
1 to: listadoFueros size do:[:i|[fuero=i nombre] 
ifTrue:[b=0. abogado fueroProcesal: fuero] ] ]].

!

altaCausa
|causa|
causa := Causa new.
causa nro_causa:(Prompter prompt: 'Ingrese un numero de causa')."falta validar que el numero sea siempre diferente"
causa fecha_ini: Date today.
causa estado: 'En curso'.
causa fueroProcesal:(Prompter prompt: 'Ingrese el fuero procesal de la causa'). "Falta validacion"
causa juzgado_radicacion:(Prompter prompt: 'Ingrese el juzgado de radicacion').
causa motivos: (Prompter prompt: 'Ingrese los motivos').
causa demandado:(Prompter prompt: 'Ingrese el nombre y apellido del demandado').
causa demandante: (Prompter prompt: 'Ingrese el nombre y apellido').
causas add: causa.!

altaFuero
|unFuero|
unFuero := Fuero new.
unFuero nombre: (Prompter prompt: 'Ingrese el nombre del fuero').
unFuero descripcion: (Prompter prompt: 'Ingrese la descripcion del fuero').
unFuero add: listadoFueros.!

altaListadoAbogado
abogados := OrderedCollection new.!

altaListadoCausas
causas := OrderedCollection new.!

altaListadoFueros
listadoFueros := OrderedCollection new.!

menu
| op |
op := 5.
[ op = 0 ] whileFalse: [
MessageBox notify: 'MENU:
1- Abrir causa
2- Cerrar causa
3- Liquidar haberes de un abogado en un periodo de fechas determinado
0- Salir'.
op:= (Prompter prompt:'Ingrese opción:') asNumber
asInteger.
( op = 1 ) ifTrue:[ self altaCausa ].

"( op = 2 ) ifTrue:[ self l1].
( op = 3 ) ifTrue: [ self l2 ]."
]! !
!Estudio categoriesForMethods!
altaAbogado!public! !
altaCausa!public! !
altaFuero!public! !
altaListadoAbogado!public! !
altaListadoCausas!public! !
altaListadoFueros!public! !
menu!public! !
!

Fuero guid: (GUID fromString: '{d2d4fccc-a9ea-47d9-911b-3da4bf33af55}')!
Fuero comment: ''!
!Fuero categoriesForClass!Kernel-Objects! !
!Fuero methodsFor!

descripcion
	^descripcion!

descripcion: anObject
	descripcion := anObject!

nombre
	^nombre!

nombre: anObject
	nombre := anObject! !
!Fuero categoriesForMethods!
descripcion!accessing!private! !
descripcion:!accessing!private! !
nombre!accessing!private! !
nombre:!accessing!private! !
!

Abogado guid: (GUID fromString: '{60af02a9-b897-4052-ab84-a19eea0bd245}')!
Abogado comment: ''!
!Abogado categoriesForClass!Kernel-Objects! !
!Abogado methodsFor!

cantCausasAbiertas
^ causasAbiertas size!

cantCausasAbiertas: anObject
	cantCausasAbiertas := anObject!

cantFalloFavorables
	^cantFalloFavorables!

cantFalloFavorables: anObject
	cantFalloFavorables := anObject!

cargaDatos
nombre := Prompter prompt: ('Ingrese el nombre del abogado').
codigo:= Abogado ultCodigo.
Abogado incrementarUltCodigo.
tipoDoc := Prompter prompt: ('Ingrese el tipo de documento del abogado').
nroDoc := Prompter prompt: ('Ingrese el numero de documento del abogado').
domicilio := Prompter prompt: ('Ingrese el domicilio del abogado').
email := Prompter prompt: ('Ingrese el email del abogado').
!

causasAbiertas
causasAbiertas := OrderedCollection new!

causasAbiertas: anObject
	causasAbiertas := anObject!

codigo
	^codigo!

domicilio
	^domicilio!

domicilio: anObject
	domicilio := anObject!

email
	^email!

email: anObject
	email := anObject!

fueroProcesal
	^fueroProcesal!

fueroProcesal: anObject
	fueroProcesal := anObject!

montoFijoXCausa
	^montoFijoXCausa!

montoFijoXCausa: anObject
	montoFijoXCausa := anObject!

nroDoc
	^nroDoc!

nroDoc: anObject
	nroDoc := anObject!

porcentajeXCausas
	^porcentajeXCausas!

porcentajeXCausas: anObject
	porcentajeXCausas := anObject!

tipoDoc
	^tipoDoc!

tipoDoc: anObject
	tipoDoc := anObject! !
!Abogado categoriesForMethods!
cantCausasAbiertas!accessing!public! !
cantCausasAbiertas:!accessing!private! !
cantFalloFavorables!accessing!private! !
cantFalloFavorables:!accessing!private! !
cargaDatos!public! !
causasAbiertas!accessing!public! !
causasAbiertas:!accessing!private! !
codigo!accessing!private! !
domicilio!accessing!private! !
domicilio:!accessing!private! !
email!accessing!private! !
email:!accessing!private! !
fueroProcesal!accessing!private! !
fueroProcesal:!accessing!private! !
montoFijoXCausa!accessing!private! !
montoFijoXCausa:!accessing!private! !
nroDoc!accessing!private! !
nroDoc:!accessing!private! !
porcentajeXCausas!accessing!private! !
porcentajeXCausas:!accessing!private! !
tipoDoc!accessing!private! !
tipoDoc:!accessing!private! !
!

!Abogado class methodsFor!

incrementarUltCodigo
UltimoCodigo := UltimoCodigo + 1.
!

iniciarUltCodigo
UltimoCodigo := 1.
!

ultCodigo
^UltimoCodigo
! !
!Abogado class categoriesForMethods!
incrementarUltCodigo!public! !
iniciarUltCodigo!public! !
ultCodigo!public! !
!

AbogadoCobroFijo guid: (GUID fromString: '{33bc375e-c134-4f62-8aba-b18cdd99ea70}')!
AbogadoCobroFijo comment: ''!
!AbogadoCobroFijo categoriesForClass!Kernel-Objects! !
!AbogadoCobroFijo methodsFor!

calcularLiquidacion
!

monto
	^monto!

monto: anObject
	monto := anObject! !
!AbogadoCobroFijo categoriesForMethods!
calcularLiquidacion!public! !
monto!accessing!private! !
monto:!accessing!private! !
!

AbogadoCobroPje guid: (GUID fromString: '{9c01284f-0746-4dc9-8d8a-7d07c681524d}')!
AbogadoCobroPje comment: ''!
!AbogadoCobroPje categoriesForClass!Kernel-Objects! !
!AbogadoCobroPje methodsFor!

calcularLiquidacion
!

porcentaje
	^porcentaje!

porcentaje: anObject
	porcentaje := anObject! !
!AbogadoCobroPje categoriesForMethods!
calcularLiquidacion!public! !
porcentaje!accessing!private! !
porcentaje:!accessing!private! !
!

"Binary Globals"!

