      subroutine HAGRE
     $(VSMLL,IPZER)
C
C     Rudolf Loeser, 1984 Sep 24
C---- Sets up "DIVIDE" controls.
C     !DASH
      save
C     !DASH
      real*8 VSMLL
      integer IPZER
C     !COM
C---- DIVIDER     as of 2003 Sep 29
      real*8      VSMLDV
      integer     MESDIV,MESDVZ,KNTDIV,KNTDVZ
      common      /DIVIDE1/ VSMLDV
      common      /DIVIDE2/ MESDIV,MESDVZ,KNTDIV,KNTDVZ
C     Parameters for subroutine "DIVIDE":
C       VSMLDV - replacement for B in A/B when B=0;
C       MESDIV - switch for A/0 error messages;
C       MESDVZ - switch for 0/0 error messages;
C       KNTDIV - number of times A/0 was detected;
C       KNTDVZ - number of times 0/0 was detected.
C     .
C     !DASH
      external HI, BYE
C
      call HI ('HAGRE')
C     !BEG
      VSMLDV = VSMLL
      KNTDIV = 0
      KNTDVZ = 0
      MESDIV = 0
      MESDVZ = 0
      if((IPZER.eq.1).or.(IPZER.eq.3)) then
        MESDIV = 1
      end if
      if((IPZER.eq.2).or.(IPZER.eq.3)) then
        MESDVZ = 1
      end if
C     !END
      call BYE ('HAGRE')
C
      return
      end
