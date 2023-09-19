

(deffacts prueba
(initial-fact)
(contador-si 0)
(delira-esquizo n)
(certezas 1 0)
(certezas 2 0)
(certezas 3 0)
(certezas 4 0)
(certezas 5 0)
(esquizofrenico n)
(sintomas x))

;--------------------------------------------------------------------------------------

(deffunction pide_cert
()
(printout t "¿Hasta qué punto está seguro? Use un porcentaje entre 0 y 100" crlf)
(bind ?respuesta (read))
(while (not (and (numberp ?respuesta) (>= ?respuesta 0) (<= ?respuesta 100)))
	(printout t "¿Hasta qué punto estás seguro? Usa un porcentaje entre 0 y 100" crlf)
	(bind ?respuesta (read)))
?respuesta)

;--------------------------------------------------------------------------------------

(deffunction toma-resp
()
(bind ?resp (read))
(while (not (or (eq ?resp s) (eq ?resp n)))
		(printout t "Por favor, conteste con <s> ó <n>" crlf)
		(bind ?resp (read)))
	?resp)


;--------------------------------------------------------------------------------------

(defrule sintomas-general
(initial-fact)
(sintomas $?sintoma)
(contador-si ?contador)
=>
(printout t crlf "    Bienvenido al Sistema Experto para diagnóstico de la Esquizofrenia." crlf
"    ¿Cree que el paciente muestra los siguientes síntomas?" crlf
"    Por favor, conteste en cada caso con (s/n):" crlf crlf)
(open "esquizo2.clp" lee "r")
(while (neq ?sintoma "fin")
	(bind ?sintoma (readline lee))
	(if (eq ?sintoma "fin")
	then
	(break))
	(printout t ?sintoma crlf)
	(bind ?resp (toma-resp))
	(assert (respuestas ?resp))
	(if (eq ?resp s)
	then
		(bind ?contador (+ ?contador 1))
		(assert (sintomas ?sintoma))
		(assert (contador-si ?contador))
		(bind ?cert (pide_cert))
		(assert (certezas ?contador ?cert))));fin while, fin if-then
(retract 0))

;--------------------------------------------------------------------------------------

(defrule diagnostica-general
(contador-si 2)
?pregunta-mas <- (delira-esquizo n)
;(certeza sintoma-si ?contador ?certeza)
=>
(assert (esquizofrenico s))
(retract ?pregunta-mas)
(printout t crlf "    El paciente cumple el criterio A del DSM-IV para la esquizofrenia" crlf))

;--------------------------------------------------------------------------------------

(defrule delirante1
(contador-si 1)
(sintomas "    1. ideas delirantes")
(certezas 1 ?certeza1)
?pregunta-mas <- (delira-esquizo n)
=>
(printout t "¿Las ideas delirantes son extrañas?" crlf)
(bind ?resp (toma-resp))
(assert (respuestas ?resp))
(if (eq ?resp s)
then
	(assert (esquizofrenico s))  
	(bind ?certeza2 (pide_cert))
	(assert (certezas 2 ?certeza2))
	(assert (delira-esquizo s))
	(retract ?pregunta-mas)
	(printout t crlf "   El paciente cumple el criterio A del DSM-IV para la esquizofrenia" crlf)))
	

;--------------------------------------------------------------------------------------

(defrule delirante2
(contador-si 1)
(sintomas "    1. ideas delirantes")
(certezas 1 ?certeza1)
?pregunta-mas <- (delira-esquizo n)
=>
(printout t "¿Las ideas delirantes consisten en una voz que comenta continuamente
los pensamientos o el comportamiento del sujeto?" crlf)
(bind ?resp (toma-resp))
(assert (respuestas ?resp))
(if (eq ?resp s)
then
	(assert (esquizofrenico s))
	(bind ?certeza2 (pide_cert))
	(assert (certezas 2 ?certeza2))
	(assert (delira-esquizo s))
	(retract ?pregunta-mas)
	(printout t crlf "    El paciente cumple el criterio A del DSM-IV para la esquizofrenia" crlf)))

;--------------------------------------------------------------------------------------

(defrule delirante3
(contador-si 1)
(sintomas "    1. ideas delirantes")
(certezas 1 ?certeza1)
?pregunta-mas <- (delira-esquizo n)
=>
(printout t "¿Hay dos o más voces que conversan entre ellas?" crlf)
(retract ?pregunta-mas)
(bind ?resp (toma-resp))
(assert (respuestas ?resp))
(if (eq ?resp s)
then
	(assert (esquizofrenico s))
	(bind ?certeza2 (pide_cert))
	(assert (certezas 2 ?certeza2))
	(assert (delira-esquizo s))
	(printout t crlf "    El paciente cumple el criterio A del DSM-IV para la esquizofrenia" crlf)))

;--------------------------------------------------------------------------------------

(defrule certeza1
(esquizofrenico s)
(certezas 1 ?certeza1)
(certezas 2 ?certeza2)
(certezas 3 ?certeza3)
(certezas 4 ?certeza4)
(certezas 5 ?certeza5)
=>
(bind ?max (max ?certeza1 ?certeza2 ?certeza3 ?certeza4 ?certeza5))
(assert (cert-max ?max)))


(defrule certeza2
(certezas 1 ?certeza1)
(certezas 2 ?certeza2)
(certezas 3 ?certeza3)
(certezas 4 ?certeza4)
(certezas 5 ?certeza5)
?esquizo <- (esquizofrenico s)
(cert-max ?max)
?quita <- (certezas ? ?max)
=>
(assert (activa-quitamax2))
(retract ?quita)
(retract ?esquizo))


(defrule certeza3
?activa <- (activa-quitamax2)
(certezas 1 ?certeza1)
(certezas 2 ?certeza2)
(certezas 3 ?certeza3)
(certezas 4 ?certeza4)
(certezas 5 ?certeza5)
=>
(bind ?cert (max ?certeza1 ?certeza2 ?certeza3 ?certeza4 ?certeza5))
(assert (certfinal ?cert))
(retract ?activa))


(defrule certeza4
(certfinal ?cert)
?esquizo <- (esquizofrenico n)
=>
(retract ?esquizo)
(close lee)
(printout t "    ...con una certeza del " ?cert "%" crlf crlf))


;--------------------------------------------------------------------------------------


(defrule no-esquizo
(esquizofrenico n)
=>
(close lee)
(printout t crlf "    El paciente no cumple el criterio A del DSM-IV para la esquizofrenia" crlf))







