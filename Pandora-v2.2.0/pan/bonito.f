      subroutine BONITO
     $(DUMP,CDL)
C
C     Rudolf Loeser, 1992 Jan 23
C---- Computes CDL for TIMOTHY when ML = MU.
C     (This is version 2 of BONITO.)
C     !DASH
      save
C     !DASH
      real*8 B1, B2, BR, CDL, FA, FB, FC, FD, FE, FF, FG, FH, FI, PA,
     $       PB, PC, PD, TWO
      integer LUEO
      logical DUMP
C     !COM
C---- TRETA       as of 1992 Jan 24
      integer     MU,ML,N1U,N1L,N2U,N2L
      real*8      FMU,FML,FN1U,FN1L,FN2U,FN2L,FUL
      dimension   FUL(7)
      common      /TRETA1/ MU,ML,N1U,N1L,N2U,N2L
      common      /TRETA2/ FMU,FML,FN1U,FN1L,FN2U,FN2L,FUL
C     Parameters for TIMOTHY and its callees.
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external FACTIN, PSI, HI, BYE
C     !EJECT
C
      call HI ('BONITO')
C     !BEG
      call FACTIN ((N1U+MU), FA)
      call FACTIN ((N2U+MU), FB)
      call FACTIN ((N1L+ML), FC)
      call FACTIN ((N2L+ML), FH)
      call FACTIN (MU , FD)
      call FACTIN (N1U, FE)
      call FACTIN (N2U, FF)
      call FACTIN (N1L, FG)
      call FACTIN (N2L, FI)
C
      call PSI (FUL(1), ML, N1U, N1L    , PA)
      call PSI (FUL(1), ML, N2U, N2L    , PB)
      call PSI (FUL(1), ML, N1U, (N1L-1), PC)
      call PSI (FUL(1), ML, N2U, (N2L-1), PD)
C
      B1 = FN1L*PC*PB-FN2L*PA*PD
      B2 = TWO*(FN1L-FN2L)*FUL(2)-(FN1U-FN2U)*FUL(7)
      BR = B2*PA*PB-TWO*B1
C
      CDL = (FUL(4)/(FD**4))*((FA/FE)*(FB/FF)*(FC/FG)*(FH/FI))*(BR**2)
C
      if(DUMP) then
        write (LUEO,100) FA,FB,FC,FH,FD,FE,FF,FG,FI,PA,PB,PC,PD,B1,B2,BR
  100   format(' ','(N1U+MU)!=',1PE16.8,5X,'(N2U+MU)!=',E16.8,5X,
     $             '(N1L+ML)!=',E16.8/
     $         ' ','(N2L+ML)!=',E16.8,5X,'(MU)!=',E16.8,5X,
     $             '(N1U)!=',E16.8/
     $         ' ','(N2U)!=',E16.8,5X,'(N1L)!=',E16.6,5X,
     $             '(N2L)!=',E16.8/
     $         ' ','psi(ML,N1U,N1L)=',E16.8,5X,'psi(ML,N2U,N2L)=',E16.8/
     $         ' ','psi(ML,N1U,N1L-1)=',E16.8,5X,
     $             'psi(ML,N2U,N2L-1)=',E16.8/
     $         ' ','brack1=',E16.8,5X,'brack2=',E16.8,5X,'brace=',E16.8)
      end if
C     !END
      call BYE ('BONITO')
C
      return
      end
