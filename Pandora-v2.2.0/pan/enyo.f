      subroutine ENYO
     $(K,KMX,IU,IL,LAB,CALLER,STOP)
C
C     Rudolf Loeser, 1991 Aug 26
C---- Checks on input counter limits, for HUDRO.
C     !DASH
      save
C     !DASH
      integer IL, IU, K, KMX, LUEO
      logical STOP
      character CALLER*(*), LAB*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, HI, BYE
C
      call HI ('ENYO')
C     !BEG
      if(K.gt.KMX) then
        call MESHED (CALLER, 1)
        write (LUEO,100) LAB,IU,IL,K,KMX
  100   format(' ',A,'(',I2,'/',I2,') =',I6,', which exceeds',I6,'.')
        STOP = .true.
      end if
C     !END
      call BYE ('ENYO')
C
      return
      end
