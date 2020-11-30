      ******************************************************************
      * Author: Gabriel Nascimento dos Santos
      * Date: 2020-11-29
      * Purpose: Maintain student records
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGCADALUNO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALUNOS ASSIGN TO 'E:\DEVELOP\COBOL\ALUNOS.DAT'
           ORGANISATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           FILE STATUS IS WRK-ALUNO-STATUS
           RECORD KEY IS ALUNO-MATRICULA.
           
           SELECT RELATO-ALUNOS 
           ASSIGN TO 'E:\DEVELOP\COBOL\RELAT-ALN.DAT'
           ORGANISATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WRK-RELATO-STATUS.
           
           
       DATA DIVISION.
       FILE SECTION.
       FD ALUNOS.
       01 ALUNO-REG.
           02 ALUNO-MATRICULA PIC 9(9).
           02 ALUNO-NOME PIC X(50).
           02 ALUNO-EMAIL PIC X(50).
           02 FILLER PIC X(75).
           
       FD RELATO-ALUNOS.
       01 RELATO-ALUNO-REG.
           02 REL-ALUNO-MATRICULA PIC 9(9).
           02 REL-ALUNO-NOME PIC X(50).
           
       WORKING-STORAGE SECTION.
       77 WRK-OPCAO PIC X(1).
       77 WRK-ALUNO-STATUS PIC 9(2) VALUE NULL.
       77 WRK-MSG PIC X(50) VALUE SPACES.
       77 WRK-VALIDO PIC X(1) VALUE 'Y'.
       77 WRK-KEY PIC X(1) VALUE SPACES.
       77 WRK-CONFIRMA-EXCLUSAO PIC X(2) VALUE SPACES.
       77 WRK-RELATO-STATUS PIC 9(2) VALUE NULL.
       77 WRK-FS-FLAG PIC X(1).
       77 WRK-CONTADOR PIC 9(9) VALUE ZEROS.
       77 WRK-LINHA-REL PIC 9(9) VALUE ZEROS.
       
       
       77 CABECALHO-MATRICULA PIC X(20)
       VALUE '--- MATRICULA ---'.  
       77 CABECALHO-NOME PIC X(50)
       VALUE '--- NOME ---'.  
       
       SCREEN SECTION.
       01 CABECALHO.
           02 BLANK SCREEN.
           02 LINE 1 COLUMN 1 ERASE EOL BACKGROUND-COLOR 2.
           02 LINE 1 COLUMN 10 VALUE 'Cadastro de Alunos'
           BACKGROUND-COLOR 2.
       01 TELA-OPCOES.
           02 LINE 09 COLUMN 10 VALUE '*** MENU PRINCIPAL ***' .
           02 LINE 10 COLUMN 10 VALUE '1 - Cadastrar novo aluno'.
           02 LINE 11 COLUMN 10 VALUE '2 - Consultar aluno'.
           02 LINE 12 COLUMN 10 VALUE '3 - Alterar aluno'.
           02 LINE 13 COLUMN 10 VALUE '4 - Excluir aluno'.
           02 LINE 14 COLUMN 10 VALUE '5 - Mostrar relatorio de alunos'.
           02 LINE 15 COLUMN 10 VALUE '6 - Gravar relatorio de alunos'.
           02 LINE 16 COLUMN 10 VALUE '7 - Fechar programa'.
           02 LINE 17 COLUMN 10 VALUE 'Opcao:......'.
           02 COLUMN PLUS 1 PIC X(2) USING WRK-OPCAO.
       
       01 TELA-CADASTRO.
           02 LINE 10 COLUMN 10 VALUE 'Matricula(*): '.
           02 COLUMN PLUS 1 PIC 9(9) USING ALUNO-MATRICULA
           BLANK WHEN ZEROS.
           02 LINE 11 COLUMN 10 VALUE 'Nome(*): '.
           02 COLUMN PLUS 1 PIC X(50) USING ALUNO-NOME.
           02 LINE 12 COLUMN 10 VALUE 'E-mail(*): '.
           02 COLUMN PLUS 1 PIC X(50) USING ALUNO-EMAIL.
           02 LINE 20 COLUMN 1 ERASE EOL BACKGROUND-COLOR 4.
           02 COLUMN PLUS 9 
           VALUE 'Os campos com (*) sao obrigatorios' 
           BACKGROUND-COLOR 4.
           
       01 TELA-PRE-CONSULTA.    
           02 LINE 10 COLUMN 10 VALUE 'Matricula(*): '.
           02 COLUMN PLUS 1 PIC 9(9) USING ALUNO-MATRICULA
           BLANK WHEN ZEROS.
           
       01 TELA-CONSULTA.
           02 LINE 10 COLUMN 10 VALUE 'Matricula(*): '.
           02 COLUMN PLUS 1 PIC 9(9) FROM ALUNO-MATRICULA.
           02 LINE 11 COLUMN 10 VALUE 'Nome(*): '.
           02 COLUMN PLUS 1 PIC X(50) FROM ALUNO-NOME.
           02 LINE 12 COLUMN 10 VALUE 'E-mail(*): '.
           02 COLUMN PLUS 1 PIC X(50) FROM ALUNO-EMAIL.
           02 LINE 20 COLUMN 1 ERASE EOL BACKGROUND-COLOR 4.
           02 LINE 24 COLUMN 1 ERASE EOL USING WRK-KEY.
           
       01 TELA-ALTERACAO.
           02 LINE 10 COLUMN 10 VALUE 'Matricula: '.
           02 COLUMN PLUS 1 PIC 9(9) FROM ALUNO-MATRICULA
           BLANK WHEN ZEROS.
           02 LINE 11 COLUMN 10 VALUE 'Nome(*): '.
           02 COLUMN PLUS 1 PIC X(50) USING ALUNO-NOME.
           02 LINE 12 COLUMN 10 VALUE 'E-mail(*): '.
           02 COLUMN PLUS 1 PIC X(50) USING ALUNO-EMAIL.
           02 LINE 20 COLUMN 1 ERASE EOL BACKGROUND-COLOR 4.
           02 COLUMN PLUS 9 
           VALUE 'Os campos com (*) sao obrigatorios' 
           BACKGROUND-COLOR 4.
           
       01 TELA-EXCLUSAO.
           02 LINE 10 COLUMN 10 VALUE 'Matricula: '.
           02 COLUMN PLUS 1 PIC 9(9) FROM ALUNO-MATRICULA
           BLANK WHEN ZEROS.
           02 LINE 11 COLUMN 10 VALUE 'Nome(*): '.
           02 COLUMN PLUS 1 PIC X(50) FROM ALUNO-NOME.
           02 LINE 12 COLUMN 10 VALUE 'E-mail(*): '.
           02 COLUMN PLUS 1 PIC X(50) FROM ALUNO-EMAIL.
           02 LINE 20 COLUMN 1 ERASE EOL BACKGROUND-COLOR 4.
           02 COLUMN PLUS 9 
           VALUE 'Deseja excluir o registro?' 
           BACKGROUND-COLOR 4.
           02 COLUMN PLUS 2 USING WRK-CONFIRMA-EXCLUSAO.
           
           
           
       01 TELA-MENSAGEM-ERRO.
           02 BLANK SCREEN.
           02 LINE 12 COLUMN 1 ERASE EOL BACKGROUND-COLOR 4.
           02 COLUMN PLUS 9 
           FROM WRK-MSG
           BACKGROUND-COLOR 4.
           02 LINE 24 COLUMN 1 USING WRK-KEY.
       
       01 TELA-MENSAGEM-SUCESSO.
           02 BLANK SCREEN.
           02 LINE 12 COLUMN 1 ERASE EOL BACKGROUND-COLOR 1.
           02 COLUMN PLUS 9 
           FROM WRK-MSG
           BACKGROUND-COLOR 1.
           02 LINE 24 COLUMN 1 USING WRK-KEY.
           
           
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 1000-INICIAR.
           PERFORM 1100-MOSTRAR-TELA-OPCOES.
           PERFORM 3000-FINALIZAR.
           
       1000-INICIAR.
           PERFORM 1001-ABRIR-ARQUIVO-ALUNOS.
       
       1001-ABRIR-ARQUIVO-ALUNOS.
           OPEN I-O ALUNOS.
           IF WRK-ALUNO-STATUS = 35 THEN
               OPEN OUTPUT ALUNOS
               CLOSE ALUNOS
               OPEN I-O ALUNOS
           END-IF.
           
       1100-MOSTRAR-TELA-OPCOES.
           MOVE SPACE TO WRK-OPCAO.
           MOVE SPACE TO WRK-CONFIRMA-EXCLUSAO.
           MOVE ZEROS TO WRK-CONTADOR.
           PERFORM 1202-LIMPAR-CAMPOS.
           DISPLAY CABECALHO.
           ACCEPT TELA-OPCOES.
           CLOSE ALUNOS.
           PERFORM 1001-ABRIR-ARQUIVO-ALUNOS.
           EVALUATE WRK-OPCAO
               WHEN 1
                   PERFORM 1200-MOSTRAR-TELA-INCLUSAO
               WHEN 2
                   PERFORM 1300-MOSTRAR-TELA-CONSULTA
               WHEN 3
                   PERFORM 1400-MOSTRAR-PRE-TELA-ALTERACAO
               WHEN 4
                   PERFORM 1500-MOSTRAR-PRE-TELA-EXCLUSAO
               WHEN 5
                   PERFORM 2200-GERAR-RELATORIO-TELA
               WHEN 6
                   PERFORM 2000-GERAR-RELATORIO-DISCO
               WHEN 7
                   PERFORM 3000-FINALIZAR
               WHEN OTHER
                   MOVE 'Opcao invalida' TO WRK-MSG
                   ACCEPT TELA-MENSAGEM-ERRO
                   PERFORM 1100-MOSTRAR-TELA-OPCOES
           END-EVALUATE.
       
       1200-MOSTRAR-TELA-INCLUSAO.
           DISPLAY CABECALHO.
           ACCEPT TELA-CADASTRO.
           PERFORM 1201-VALIDAR-ALUNO.
           IF WRK-VALIDO EQUAL 'Y' THEN 
               WRITE ALUNO-REG
                   INVALID KEY
                       MOVE 'Ja existe um aluno com essa matricula'
                       TO WRK-MSG
                       ACCEPT TELA-MENSAGEM-ERRO
                       PERFORM 1200-MOSTRAR-TELA-INCLUSAO
                   NOT INVALID KEY 
                       MOVE 'Registro salvo com sucesso' 
                       TO WRK-MSG
                       ACCEPT TELA-MENSAGEM-SUCESSO
                       PERFORM 1100-MOSTRAR-TELA-OPCOES
               END-WRITE
           ELSE 
               ACCEPT TELA-MENSAGEM-ERRO
               PERFORM 1200-MOSTRAR-TELA-INCLUSAO
           END-IF.
       1201-VALIDAR-ALUNO.
           IF ALUNO-MATRICULA EQUAL 0 
               OR ALUNO-NOME  EQUAL SPACES 
               OR ALUNO-EMAIL EQUAL SPACES THEN
               MOVE '-------- Digite os campos obrigatorios --------' 
               TO WRK-MSG
               MOVE 'N' TO WRK-VALIDO
           ELSE
               MOVE SPACES TO WRK-MSG
               MOVE 'Y' TO WRK-VALIDO
           END-IF.
       1202-LIMPAR-CAMPOS.
           MOVE ZEROS TO ALUNO-MATRICULA.
           MOVE SPACES TO ALUNO-NOME.
           MOVE SPACES TO ALUNO-EMAIL.
           
       1300-MOSTRAR-TELA-CONSULTA.
           DISPLAY CABECALHO.
           ACCEPT TELA-PRE-CONSULTA.
           READ ALUNOS.
           IF WRK-ALUNO-STATUS = 23 THEN
               MOVE 'Aluno nao encontrado' TO WRK-MSG
               ACCEPT TELA-MENSAGEM-ERRO
           ELSE
               ACCEPT TELA-CONSULTA
           END-IF.
           PERFORM 1100-MOSTRAR-TELA-OPCOES.
           
       1400-MOSTRAR-PRE-TELA-ALTERACAO.
           DISPLAY CABECALHO.
           ACCEPT TELA-PRE-CONSULTA.
           READ ALUNOS.
           IF WRK-ALUNO-STATUS = 23 THEN
               MOVE 'Aluno nao encontrado' TO WRK-MSG
               ACCEPT TELA-MENSAGEM-ERRO
           ELSE
               PERFORM 1410-MOSTRAR-TELA-ALTERACAO
           END-IF.
           PERFORM 1100-MOSTRAR-TELA-OPCOES.
           
       1410-MOSTRAR-TELA-ALTERACAO.
           DISPLAY CABECALHO.
           ACCEPT TELA-ALTERACAO.
               
           PERFORM 1201-VALIDAR-ALUNO.
               
           IF WRK-VALIDO EQUAL 'Y' THEN  
               REWRITE ALUNO-REG
                   INVALID KEY 
                      MOVE 'Aluno nao encontrado ao alterar'
                      TO WRK-MSG
                   ACCEPT TELA-MENSAGEM-ERRO
                   NOT INVALID KEY
                       MOVE 'Registro alterado com sucesso'
                       TO WRK-MSG
                       ACCEPT TELA-MENSAGEM-SUCESSO
               END-REWRITE
           ELSE 
               ACCEPT TELA-MENSAGEM-ERRO
               PERFORM 1410-MOSTRAR-TELA-ALTERACAO
           END-IF.
               
       1500-MOSTRAR-PRE-TELA-EXCLUSAO.
           DISPLAY CABECALHO.
           ACCEPT TELA-PRE-CONSULTA.
           READ ALUNOS.
           IF WRK-ALUNO-STATUS = 23 THEN
               MOVE 'Aluno nao encontrado' TO WRK-MSG
               ACCEPT TELA-MENSAGEM-ERRO
           ELSE
               DISPLAY CABECALHO
               PERFORM 1510-MOSTRAR-TELA-EXCLUSAO
           END-IF.
           PERFORM 1100-MOSTRAR-TELA-OPCOES.
           
       1510-MOSTRAR-TELA-EXCLUSAO.
           DISPLAY CABECALHO.
           ACCEPT TELA-EXCLUSAO.
           MOVE FUNCTION UPPER-CASE(WRK-CONFIRMA-EXCLUSAO) 
           TO WRK-CONFIRMA-EXCLUSAO.
           IF WRK-CONFIRMA-EXCLUSAO = 'S' THEN
               DELETE ALUNOS
                   INVALID KEY
                       MOVE 'Aluno nao encontrado' TO WRK-MSG
                       ACCEPT TELA-MENSAGEM-ERRO
                   NOT INVALID KEY
                       MOVE 'Aluno excluido com sucesso' TO WRK-MSG
                       ACCEPT TELA-MENSAGEM-SUCESSO
               END-DELETE
           ELSE
               IF WRK-CONFIRMA-EXCLUSAO = 'N' THEN
                   PERFORM 1100-MOSTRAR-TELA-OPCOES
               ELSE 
                   MOVE 'Opcao invalida' TO WRK-MSG
                   ACCEPT TELA-MENSAGEM-ERRO
                   PERFORM 1510-MOSTRAR-TELA-EXCLUSAO
               END-IF
           END-IF.
       
       
       2000-GERAR-RELATORIO-DISCO.
           PERFORM 2100-ABRIR-ARQUIVO-RELAT.
           CLOSE ALUNOS.
           PERFORM 1001-ABRIR-ARQUIVO-ALUNOS.
           PERFORM UNTIL WRK-FS-FLAG = 'Y'
               READ ALUNOS
                   AT END MOVE 'Y' TO WRK-FS-FLAG
                   NOT AT END
                   MOVE ALUNO-MATRICULA TO REL-ALUNO-MATRICULA
                   MOVE ALUNO-NOME TO REL-ALUNO-NOME
                   WRITE RELATO-ALUNO-REG
               END-READ
           END-PERFORM.
               
           MOVE SPACE TO WRK-FS-FLAG.
           
           CLOSE RELATO-ALUNOS.
           CLOSE ALUNOS.
           PERFORM 1001-ABRIR-ARQUIVO-ALUNOS.
           MOVE 'Relatorio gerado com sucesso' TO WRK-MSG.
           ACCEPT TELA-MENSAGEM-SUCESSO.
           PERFORM 1100-MOSTRAR-TELA-OPCOES.
       
       2100-ABRIR-ARQUIVO-RELAT.
           OPEN OUTPUT RELATO-ALUNOS.
           IF WRK-RELATO-STATUS = 35 THEN
               OPEN OUTPUT RELATO-ALUNOS
               CLOSE RELATO-ALUNOS
               OPEN OUTPUT RELATO-ALUNOS
           END-IF.
               
       2200-GERAR-RELATORIO-TELA.
           CLOSE ALUNOS.
           PERFORM 1001-ABRIR-ARQUIVO-ALUNOS.
           
           PERFORM 2201-MONTA-CABECALHO.
           
           MOVE 12 TO WRK-LINHA-REL.
           PERFORM UNTIL WRK-FS-FLAG = 'Y'
               READ ALUNOS
                   AT END MOVE 'Y' TO WRK-FS-FLAG
                   NOT AT END
                   DISPLAY ALUNO-MATRICULA AT LINE WRK-LINHA-REL
                   COLUMN 10
                   DISPLAY ALUNO-NOME AT LINE WRK-LINHA-REL 
                   COLUMN 30
                   ADD 1 TO WRK-CONTADOR
                   IF WRK-CONTADOR < 5 
                       AND WRK-FS-FLAG NOT EQUAL 'Y' THEN
                       COMPUTE WRK-LINHA-REL = (WRK-LINHA-REL + 1)
                   ELSE
                      MOVE ZERO TO WRK-CONTADOR
                      MOVE 12 TO WRK-LINHA-REL
                      DISPLAY 'Aperte uma tecla para a proxima pagina'
                      LINE 23 COLUMN 10 BACKGROUND-COLOR 1
                      ACCEPT WRK-KEY AT LINE 24 COLUMN 1
                      PERFORM 2201-MONTA-CABECALHO
                   END-IF
               END-READ
           END-PERFORM.
               
           MOVE SPACE TO WRK-FS-FLAG.
           CLOSE ALUNOS.
           PERFORM 1001-ABRIR-ARQUIVO-ALUNOS.
           DISPLAY 'Aperte uma tecla para voltar para a tela de opcoes'
           LINE 23 COLUMN 10 BACKGROUND-COLOR 1
           ACCEPT WRK-KEY AT LINE 24 COLUMN 1
           PERFORM 1100-MOSTRAR-TELA-OPCOES.
           
       2201-MONTA-CABECALHO.
           DISPLAY CABECALHO.
           DISPLAY '-------- RELATORIO DE ALUNOS --------'
           AT LINE 10 COLUMN 10.
           
           DISPLAY CABECALHO-MATRICULA AT LINE 11 COLUMN 10.
           DISPLAY CABECALHO-NOME AT LINE 11 COLUMN 30.
           
       3000-FINALIZAR.
           CLOSE ALUNOS.
           STOP RUN.
       END PROGRAM PROGCADALUNO.

