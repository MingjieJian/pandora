      subroutine NYASA
     $(NL,POPK,KODE,KODI)
C
C     Rudolf Loeser, 1978 Sep 27
C---- Computes switch settings, for NAMUR.
C     Returns with KODE=1 if POPK exists,              =0 otherwise.
C     Returns with KODI=1 if intermediates are needed, =0 otherwise.
C     !DASH
      save
C     !DASH
      real*8 POPK, ZERO
      integer KODE, KODI, NL
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
C               POPK(N)
      dimension POPK(*)
C
C
      call HI ('NYASA')
C     !BEG
      KODE = 1
      if(POPK(1).lt.ZERO) then
        KODE = 0
      end if
C
      KODI = 0
      if((KODE.le.0).or.((KODE.gt.0).and.(NL.le.0))) then
        KODI = 1
      end if
C     !END
      call BYE ('NYASA')
C
      return
      end
