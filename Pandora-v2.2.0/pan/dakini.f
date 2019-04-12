      subroutine DAKINI
     $(OPAC,F,XSHL,TOPT,TSHL,KODE,IMG,W)
C
C     Rudolf Loeser, 1981 Nov 05
C---- Computes Shell Ray Optical Depths.
C     !DASH
      save
C     !DASH
      real*8 F, OPAC, TSHL, W, XSHL
      integer I, IMG, J, KODE, NRP, NRPMX, NSHL
      logical TOPT
      character LAB*14
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 4),NSHL )
      equivalence (LEST( 8),NRPMX)
C     !DASH
      external TATAR, BARBARA, DIDO, HI, BYE
C
      dimension W(*)
C
C               XSHL(NRPMX,NSHL), TSHL(NRPMX,NSHL), OPAC(NRPMX), IMG(N),
      dimension XSHL(NRPMX,*),    TSHL(NRPMX,*),    OPAC(*),     IMG(*),
C
C               F(NRPMX)
     $          F(*)
C     !EJECT
C
      call HI ('DAKINI')
C     !BEG
      I  = 0
C
      do 101 J = 1,NSHL
C
        call TATAR   (I)
        NRP = 2*I+5
C
        call BARBARA (OPAC,I,F)
C
        write (LAB,100) J,I
  100   format('Shell ',I3,'(',I3,')')
        call DIDO    (XSHL(1,J),F,NRP,TSHL(1,J),KODE,TOPT,LAB,IMG,W)
        if(KODE.eq.0) then
          goto 102
        end if
  101 continue
C
  102 continue
C     !END
      call BYE ('DAKINI')
C
      return
      end
