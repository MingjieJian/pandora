      subroutine NYANZA
     $(NL1,POP1K,KODE1,NL2,POP2K,KODE2,KODI)
C
C     Rudolf Loeser, 1978 Sep 27
C---- Computes switch settings, for NOYON.
C     Returns with KODE1=1 if POP1K exists,             =0 otherwise.
C     Returns with KODE2=1 if POP2K exists,             =0 otherwise.
C     Returns with KODI =1 if intermediates are needed, =0 otherwise.
C     !DASH
      save
C     !DASH
      real*8 POP1K, POP2K, ZERO
      integer KODE1, KODE2, KODI, NL1, NL2
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HI, BYE
C
C               POP1K(N), POP2K(N)
      dimension POP1K(*), POP2K(*)
C
      call HI ('NYANZA')
C     !BEG
      KODE1 = 1
      if(POP1K(1).lt.ZERO) then
        KODE1 = 0
      end if
C
      KODE2 = 1
      if(POP2K(1).lt.ZERO) then
        KODE2 = 0
      end if
C
      KODI = 0
      if((KODE1.eq.0).or.(KODE2.eq.0).or.(NL1.le.0).or.(NL2.le.0)) then
        KODI = 1
      end if
C     !END
      call BYE ('NYANZA')
C
      return
      end
