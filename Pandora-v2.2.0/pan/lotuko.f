      subroutine LOTUKO
     $(LNRM,RAYL,KODE,I)
C
C     Rudolf Loeser, 1981 Dec 01
C---- Processes error flags for Orion Data Blocks.
C     !DASH
      save
C     !DASH
      real*8 RAYL, ZERO
      integer I, KODE, LNRM
C     !COM
C---- URANUS      as of 2005 Dec 12
      integer     LEMUR
      parameter   (LEMUR=30000)
      integer     LIMPID,IUOP,ILOP,NBOP,MBOP,KEROP,INDOP
      real*8      OPNAM
      dimension   OPNAM(LEMUR),KEROP(LEMUR),INDOP(LEMUR)
      common      /URANUS1/ LIMPID,IUOP,ILOP,NBOP,MBOP
      common      /URANUS2/ KEROP
      common      /URANUS3/ INDOP
      common      /URANUS4/ OPNAM
C     Record index for Diana/Orion Data Blocks.
C
C     KEROP .eq. 0: acceptable data block, use it;
C     KEROP .gt. 0: defective data block, do not use it.
C     (The various values of KEROP signify different error conditions).
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external HI, BYE
C
      call HI ('LOTUKO')
C     !BEG
      KODE = 0
C
      if(LNRM.eq.0) then
        if(RAYL.eq.ZERO) then
          KODE = 3
        else
          KODE = 1
        end if
      else
        if(RAYL.eq.ZERO) then
          KODE = 2
        end if
      end if
C
      KEROP(I) = KODE
C     !END
      call BYE ('LOTUKO')
C
      return
      end
