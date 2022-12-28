Progetto Linguaggi di Programmazione: JSON Parser
15 Gennaio 2023

Gruppo:
875068 Viviana Giuliani
869310 Daniel Marco Gatti

Il progetto si occupa di analizzare una stringa e trasformarla in un JSON 
fornendo prima un modulo compatibile in Lisp. 
Può recuperare un determinato valore da un JSON dato un elenco di parametri.
Scrive e legge un JSON su/da un file.

1) jsonread(filename), per ottenere la forma compatibile in Lisp del file.json
2) jsondump(json, filename), il json verrà scritto in un file con nome filename.

Si può utilizzare il programma anche con una stringa data chiamando jsonparse(json).