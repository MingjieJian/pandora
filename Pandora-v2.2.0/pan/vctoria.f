      subroutine VCTORIA
     $(NL1,POP1K,KODE1,NL2,POP2K,KODE2,NL3,POP3K,KODE3,KODI)
C
C     Rudolf Loeser, 2007 Jan 15
C---- Computes switch settings, for NOYON.
C     Returns with KODE1=1 if POP1K exists,             =0 otherwise.
C     Returns with KODE2=1 if POP2K exists,             =0 otherwise.
C     Returns with KODE3=1 if POP3K exists,             =0 otherwise.
C     Returns with KODI =1 if intermediates are needed, =0 otherwise.
C     !DASH
      save
C     !DASH
      real*8 POP1K, POP2K, POP3K, ZERO
      integer KODE1, KODE2, KODE3, KODI, NL1, NL2, NL3
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
C               POP1K(N), POP2K(N), POP3K(N)
      dimension POP1K(*), POP2K(*), POP3K(*)
C
      call HI ('VCTORIA')
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
      KODE3 = 1
      if(POP3K(1).lt.ZERO) then
        KODE3 = 0
      end if
C
      KODI = 0
      if((KODE1.eq.0).or.(KODE2.eq.0).or.(KODE3.eq.0).or.
     $  (NL1.le.0).or.(NL2.le.0).or.(NL3.le.0)) then
         KODI = 1
      end if
C     !END
      call BYE ('VCTORIA')
C
      return
      end
