      subroutine ANYULA
     $(I,L,WN,XLCOA,XLCOB,KODE,LTYPE,ISO,NB,W,J,K,M,DUMP)
C
C     Rudolf Loeser, 1992 Sep 15
C---- Selects CO-line data, for ADELMA.
C
C     KODE = 0 for just W values;
C           +1 for j -> j+1 "up";
C           -1 for j -> j-1 "down".
C
C     (This is version 2 of ANYULA.)
C     !DASH
      save
C     !DASH
      real*8 W, WN, XLCOA, XLCOB, XLM
      integer I, ISO, J, K, KODE, L, LTYPE, LUEO, M, NB
      logical DUMP, YES
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external WANDA, WITHIN, HALT, HI, BYE
C
C               W(NB), J(NB), K(NB), M(NB)
      dimension W(*),  J(*),  K(*),  M(*)
C     !EJECT
C
      call HI ('ANYULA')
C     !BEG
      call WANDA      (WN, XLM)
      call WITHIN     (XLCOA, XLM, XLCOB, 0, YES)
      if(YES) then
        NB    = NB+1
        W(NB) = XLM
C
        if(KODE.ne.0) then
C
          if(KODE.eq.+1) then
            J(NB) = (I-1)+500
            K(NB) = L-1
            M(NB) = LTYPE
          else if(KODE.eq.-1) then
            J(NB) = I
            K(NB) = L-1
            M(NB) = LTYPE
          end if
C
          if(ISO.eq.2) then
            K(NB) = K(NB)+500
          else if(ISO.ne.1) then
            write (MSSLIN(1),100) ISO
  100       format('ISO =',I12,' which does not = 1 or 2.')
            call HALT ('ANYULA', 1)
          end if
C
        end if
C
        if(DUMP) then
          if(KODE.ne.0) then
            write (LUEO,101) I,L,WN,XLCOA,XLM,XLCOB,NB,W(NB),
     $                       J(NB),K(NB),M(NB)
          else
            write (LUEO,101) I,L,WN,XLCOA,XLM,XLCOB,NB,W(NB)
          end if
  101     format(' ','  ANYULA',2I5,1PE14.7,2X,3E14.7,I6,E14.7,
     $               :,3I8)
        end if
      end if
C     !END
      call BYE ('ANYULA')
C
      return
      end
