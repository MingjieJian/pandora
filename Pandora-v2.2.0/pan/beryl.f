      subroutine BERYL
     $(MIK,ISR,KODE,I)
C
C     Rudolf Loeser, 1982 Mar 30
C---- Processes error flags, for VULCAN.
C     !DASH
      save
C     !DASH
      integer I, ISR, KODE, MIK
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
C     !DASH
      external HI, BYE
C
      call HI ('BERYL')
C     !BEG
      KODE = 0
C
      if(MIK.gt.0) then
        KODE = 1
      else
        if(ISR.le.0) then
          KODE = 2
        end if
      end if
C
      KEROP(I) = KODE
C     !END
      call BYE ('BERYL')
C
      return
      end
