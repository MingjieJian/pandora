      subroutine ANAKLID
     $(LU,IU,IL,K,N,P,SET,XND,BDI,STMA,STMB,STMAS,STMBS)
C
C     Rudolf Loeser, 2004 Oct 04
C---- Prints for AZPRIN.
C     !DASH
      save
C     !DASH
      real*8 BDI, P, SET, STMA, STMAS, STMB, STMBS, XND
      integer I, IL, IU, K, LU, N
      character BLANK*1, LAB*24, MA*1, MB*1, STAR*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(45),STAR  )
C     !DASH
C     !EJECT
      external LINER, SHIM, HI, BYE
C
C               BDI(N,NL), P(NSL), SET(N), STMA(N), STMBS(N), STMAS(N),
      dimension BDI(N,*),  P(*),   SET(*), STMA(*), STMBS(*), STMAS(*),
C
C               STMB(N), XND(N,NL)
     $          STMB(*), XND(N,*)
C
      dimension LAB(2)
C
      data LAB(1) /'A selected              '/
      data LAB(2) /'              B selected'/
C
      call HI ('ANAKLID')
C     !BEG
      if(LU.gt.0) then
        call LINER  (2, LU)
        write (LU,100) IU,IL,IU,P(IU),IL,P(IL),LAB(K),IU,IL,IU,IL
  100   format(' ','(To omit these STIM details, set LSTMP = 0.)'///
     $         ' ','Calculation of STIM for GTN(',I2,'/',I2,')',10X,
     $             'P(',I2,') =',1PE10.2,5X,'P(',I2,') =',E10.2,16X,A//
     $         ' ',15X,'SET',8X,'N(',I2,')',8X,'N(',I2,')',8X,'b(',
     $             I2,')',8X,'b(',I2,')',7X,'STIM-A',7X,'STIM-B',
     $             3X,'A-smoothed',4X,'B-smoothed')
        call LINER  (1, LU)
        do 102 I = 1,N
          MA = BLANK
          if(STMA(I).ne.STMAS(I)) then
            MA = STAR
          end if
          MB = BLANK
          if(STMB(I).ne.STMBS(I)) then
            MB = STAR
          end if
          write (LU,101) I,SET(I),XND(I,IU),XND(I,IL),BDI(I,IU),
     $                   BDI(I,IL),STMA(I),STMB(I),STMAS(I),MA,
     $                   STMBS(I),MB
  101     format(' ',I5,1P8E13.5,A1,E13.5,A1)
          call SHIM (I, 5, LU)
  102   continue
      end if
C     !END
      call BYE ('ANAKLID')
C
      return
      end
