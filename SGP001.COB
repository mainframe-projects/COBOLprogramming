      ******************************************************************
      * Author: LÁZARO V.A. SANTOS
      * Date: 14-09-2019
      * Purpose: Sistema de pastelaria
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID. SGP001.
       AUTHOR. LÁZARO VINICIUS ALMEIDA DOS SANTOS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
        INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADASTRODECEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODIGO-CEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CADCODIGO-CEP
                                            WITH DUPLICATES.

       DATA DIVISION.
       FILE SECTION.
       LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CEPCLIENTE.DAT".
               01 REGCEP.
               03 CODIGO.
                   05 CODIGO-CEP     PIC 9(08).
                03 LOGRADOURO        PIC X(40).
                03 BAIRRO       PIC X(20).


                03 CIDADE            PIC X(20).
                03 UF                PIC X(20).

      *
      *-----------------------------------------------------------------
       SCREEN SECTION.

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Bem-vindo ao SGP"
            STOP RUN.
       END PROGRAM SGP001.
