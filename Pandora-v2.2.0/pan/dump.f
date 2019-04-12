      subroutine DUMP
     $(TAU,XVAL,IBEG,IEND,TIT,KODE)
C
C     Rudolf Loeser, 1982 May 14
C---- Attempts to set up log(Tau) as a graph ordinate.
C     Returns with KODE=1 is successful, =0 otherwise.
C     !DASH
      save
C     !DASH
      real*8 TAU, XVAL, ZERO
      integer I, IBEG, IEND, KNT, KODE, NGT
      character TIT*10
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external PLUSD, LOGO, HI, BYE
C
C               TAU(N), XVAL(N)
      dimension TAU(*), XVAL(*)
C
      call HI ('DUMP')
C     !BEG
      KODE = 0
C
C---- Make sure all Tau's .gt. 0
      KNT = IEND-(IBEG-1)
      call PLUSD  (TAU(IBEG), 1, KNT, NGT)
C
      if(NGT.eq.KNT) then
C----   Set up log(Tau)
        call LOGO (TAU(IBEG), KNT, 1, ZERO, XVAL(IBEG))
        KODE = 1
        TIT  = 'log10(Tau)'
      end if
C     !END
      call BYE ('DUMP')
C
      return
      end
