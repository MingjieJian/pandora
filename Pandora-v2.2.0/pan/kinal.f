      subroutine KINAL
     $(N,TE,VM,XNU,ROOT)
C
C     Rudolf Loeser, 2003 Jan 08
C---- Computes Doppler width term, for higher H Ly lines.
C     !DASH
      save
C     !DASH
      real*8 CMPKM, ONE, ROOT, RT, TE, VM, XNU, dummy1, dummy2
      integer N
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 5),CMPKM )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HYDATA, DOPPLER, HI, BYE
C
      call HI ('KINAL')
C     !BEG
      call HYDATA  (N,XNU,dummy1,dummy2)
      call DOPPLER (XNU,TE,ONE,(VM**2),RT,dummy1)
      ROOT = CMPKM*RT
C     !END
      call BYE ('KINAL')
C
      return
      end
