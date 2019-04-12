      subroutine RADNOR
     $(N,HINT,ROSST,DBH,IMG,W)
C
C     Rudolf Loeser, 1987 Mar 20
C---- Computes DBH, for BRABANT.
C     !DASH
      save
C     !DASH
      real*8 ADD, DBH, HINT, ROOT3, ROSST, THREE, W
      integer IMG, LGT, N, jummy
      logical lummy1, lummy2
      character LABEL*30
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 4),THREE )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 8),ROOT3 )
C     !DASH
      external PLUSD, BUSH, TUNA, CONMUL, CONADD, HI, BYE
C
      dimension W(*)
C
C               HINT(N), ROSST(N), DBH(N), IMG(N)
      dimension HINT(*), ROSST(*), DBH(*), IMG(*)
C
      data LABEL /'DBH = integral of HINT(ROSST)'/
C     !EJECT
C
      call HI ('RADNOR')
C     !BEG
      call PLUSD  (HINT,1,N,LGT)
      if(LGT.lt.N) then
        call BUSH (ROSST,1,HINT,1,DBH,1,N)
      else
        call TUNA (N,ROSST,HINT,DBH,LABEL,jummy,lummy1,lummy2,IMG,W)
      end if
C
      ADD = ROOT3*HINT(1)
      call CONMUL (THREE,DBH,N)
      call CONADD (ADD  ,DBH,N)
C     !END
      call BYE ('RADNOR')
C
      return
      end
