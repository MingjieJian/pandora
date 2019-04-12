      subroutine IVAN
     $(XNUK,XNU,NPQ,NSL)
C
C     Rudolf Loeser, 2006 Apr 25
C---- Sets up frequencies for Hydrogen.
C     !DASH
      save
C     !DASH
      real*8 XNU, XNUK, ZERO, dummy1, dummy2
      integer J, JDNUK, JDXNU, NPQ, NSL
C     !COM
C---- APOLLO      as of 2006 Dec 04
      integer     MEST
      dimension   MEST(28)
      common      /APOLLO/ MEST
C     Atomic model parameter default values indicators
      equivalence (MEST(17),JDNUK)
      equivalence (MEST(16),JDXNU)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HYDATA, HI, BYE
C
C               XNU(NSL), NPQ(NSL)
      dimension XNU(*),   NPQ(*)
C
      call HI ('IVAN')
C     !BEG
C---- Frequency of continuum
      if(XNUK.le.ZERO) then
        call HYDATA   (0, XNUK, dummy1, dummy2)
        JDNUK = JDNUK+1
      end if
C---- Level frequencies
      if(XNU(1).ne.ZERO) then
        XNU(1) = ZERO
      end if
      do 100 J = 2,NSL
        if(XNU(J).le.ZERO) then
          call HYDATA (NPQ(J), XNU(J), dummy1, dummy2)
          JDXNU = JDXNU+1
        end if
  100 continue
C     !END
      call BYE ('IVAN')
C
      return
      end
