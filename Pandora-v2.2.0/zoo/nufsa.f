      subroutine NUFSA
     $(STATE,FIELD,NCAR,N,V,S)
C     Rudolf Loeser, 1986 Sep 24
C---- Decodes the current field, for NUDEAL.
C
C---- The field-decoding algorithm is a finite-state-automaton (FSA)
C     having five states, and dealing with characters classified into
C     five modes, as follows:
C
C     STATE                           MODE
C
C     1 alphanumeric                  1 digit
C     2 integer                       2 sign
C     3 exponent                      3 decimal point
C     4 floating point                4 exponent field flag
C     5 (initial)                     5 other
C
C     State transitions occur as shown in the following transition
C     matrix, whose entries specify the next state of the FSA when a
C     character of a particular mode is encountered while the FSA is in
C     the current state:
C
C                MODE=1  MODE=2  MODE=3  MODE=4  MODE=5
C
C     STATE=1       1       1       1       1       1
C     STATE=2       2       1       4       3       1
C     STATE=3       3       3       1       1       1
C     STATE=4       4       1       1       3       1
C     STATE=5       2       2       4       1       1
C
C     !DASH
      save
C     !DASH
      real*8 FONE, FTEN, S, V, VAL
      integer I, LSGN, MODE, N, NCAR, STATE
      character FIELD*(*)
C     !DASH
      external NUCLAS
C
      dimension V(3), S(3), N(3)
C
      data FONE, FTEN /1.D0, 1.D1/
C
C     !BEG
C---- LSGN will =1 for as long as a sign may legally occur
C     (an illegally-occurring sign causes the field to be considered
C     to be of type 'alphanumeric').
      LSGN = 1
C---- Loop over all characters (except terminating blank)
      do 999 I = 1,(NCAR-1)
C
C----   Classify next character
        call NUCLAS (FIELD(I:I),MODE,VAL)
        goto (100,200,300,400,500), STATE
C     !EJECT
C       ---------- A L P H A N U M E R I C
  100   continue
          STATE = 1
          goto 999
C       ---------- I N T E G E R
  200   continue
          goto (201,100,501,401,100), MODE
  201     continue
            N(1)  = N(1)+1
            V(1)  = VAL+V(1)*FTEN
            STATE = 2
            goto 999
C       ---------- E X P O N E N T
  300   continue
          goto (301,302,100,100,100), MODE
  301     continue
            V(3) = VAL+V(3)*FTEN
            LSGN = 0
            goto 999
  302     continue
          if(LSGN.eq.1) then
            LSGN = 0
            S(3) = VAL
            goto 999
          else
            goto 100
          end if
C       ---------- F L O A T I N G   P O I N T
  400   continue
          goto (402,100,100,401,100), MODE
  401     continue
            STATE = 3
            LSGN  = 1
            goto 999
  402     continue
            N(2) = N(2)+1
            V(2) = VAL+V(2)*FTEN
            goto 999
C       ---------- I N I T I A L
  500   continue
          goto (201,502,501,100,100), MODE
  501     continue
            STATE = 4
            goto 999
  502     continue
            S(1)  = VAL
            STATE = 2
            goto 999
C----
  999 continue
C     !END
C
      return
      end
