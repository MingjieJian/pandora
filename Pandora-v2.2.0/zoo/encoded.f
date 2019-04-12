      subroutine ENCODED
     $(VAL, FIELD, L,MAXSF,IZERO, NF)
C
C     Rudolf Loeser, 1991 Aug 01
C---- Encodes VAL into FIELD (length L characters), right-justified.
C     Provides as many significant figures as possible but not more
C     than min(MAXSF,MAXTY). Discards nonsignificant zeros.
C     Returns NF, the number of characters used.
C
C---- When IZERO = 1, then encodes zero as "0";
C     otherwise, encodes zero as blank.
C     !DASH
c--SGK Apr 17 2009 - catch and eliminate '0X' when NB==0
      save
C     !DASH
      real*8 AVAL, EV, HNDRD, ONE, P, TEN, TENTH, THSND, VAL, ZERO
      integer IS, IZERO, J, L, MAXSF, MAXTY, MUX, N, NB, NF, NR
      logical EOK, FOK
      character ASTAR*1, BLANK*1, FIELD*(*), FORMAT*14, ZZERO*1
C     !DASH
      intrinsic aint, min, max
C
      data ZERO,  TENTH,  ONE,   TEN,   HNDRD,  THSND /
     $     0.D0,  1.D-1,  1.D0,  1.D1,  1.D2,   1.D3  /
C
      data BLANK, ZZERO, ASTAR /' ', '0', '*'/
      data MAXTY /15/
C
C     !BEG
      NF = 0
      if(L.gt.0) then
C----   Initialize output FIELD
        FIELD(:L)=BLANK
C
        if(VAL.eq.ZERO) then
C----     Special case
          if(IZERO.eq.1) then
            FIELD(L:L) = ZZERO
          end if
          NF = 1
        else
C----     General case
C         Set up sign counter, and absolute value
          AVAL = VAL
          IS   = 0
          if(VAL.lt.ZERO) then
            AVAL = -VAL
            IS   =  1
          end if
C
C----     Set up limit for number of significant figures
          MUX = min(MAXTY,MAXSF)
C----     Compute minimum field width required for F-format
          J = log10(AVAL)+ONE
          N = max(1,J)
C----     Set up flags FOK and EOK
          FOK = (L.ge.(N+(1+IS))).and.(AVAL.ge.TENTH)
     $                           .and.(AVAL.lt.THSND)
          EOK = (L.ge.(6+IS))
C     !EJECT
          if(FOK) then
C
C----       Use F-format
C           Construct Fortran FORMAT statement ( nb X, F nf . nr )
            NR = min((L-J-(1+IS)),(MUX-J))
            NF = NR+(1+IS)+J
            NB = L-NF
c--SGK      catch and eliminate '0X'
            if (NB.GT.0) then
              write (FORMAT,100) NB,NF,NR
            else
              write (FORMAT,9100) NF,NR
            endif 
  100       format('(',I2,'X,F',I2,'.',I2,')  ')
 9100       format(        '(F',I2,'.',I2,')  ')
C----       Encode VAL
            P  = TEN**NR
            EV = aint(VAL*P)
            EV = EV/P
            write (FIELD(:L),FORMAT) EV
  101       continue
C----         Remove non-significant zeros
              if(FIELD(L:L).eq.ZZERO) then
                FIELD(:L) = BLANK//FIELD(:L-1)
                NF = NF-1
                go to 101
              end if
          else if(EOK) then
C
C----       Use E-format
C           Construct Fortran FORMAT statement ( nb X, 1PE nf . nr )
            NR = min((L-(6+IS)),(MUX-1))
            NF = NR+(6+IS)
            NB = L-NF
c--SGK      catch and eliminate '0X'
            if (NB.GT.0) then
              write (FORMAT,102) NB,NF,NR
            else
              write (FORMAT,9102) NF,NR
            endif
  102       format('(',I2,'X,1PE',I2,'.',I2,')')
 9102       format(        '(1PE',I2,'.',I2,')')
C----       Encode VAL
            write (FIELD(:L),FORMAT) VAL
  103       continue
C----         Remove nonsignificant zeroes
              if(FIELD(L-4:L-4).eq.ZZERO) then
                FIELD(:L) = BLANK//FIELD(:L-5)//FIELD(L-3:L)
                NF = NF-1
                go to 103
              end if
          else
C
C----       FIELD is not wide enough for either format - give up
            FIELD(L:L) = ASTAR
            NF = 1
          end if
        end if
      end if
C     !END
C
      return
      end
