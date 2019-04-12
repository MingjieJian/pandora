      subroutine INOPOS
     $(NO,IMAGE,IU,IL,II)
C
C     Rudolf Loeser, 2001 Dec 27
C---- Plots, for BIRYUSA.
C     (This is version 2 of INOPOS.)
C     !DASH
      save
C     !DASH
      integer I, II, IL, IU, NO
      character IMAGE*(*)
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
C     !DASH
      external ABJECT, LINER, KPRINT, HI,BYE
C
C     dimension II(5)
      dimension II(*)
C
      call HI ('INOPOS')
C     !BEG
C---- Write heading
      call ABJECT (NO)
      write (NO,101) IU,IL
  101 format(' ','Plot of log(H) vs. Delta-Lambda for transition (',I2,
     $           '/',I2,').')
      call LINER  (1,NO)
C---- Print graph
      call KPRINT (IMAGE,NO)
C---- Write label
      call LINER  (1,NO)
      write (NO,102) (ALPHS(I),II(I),I=1,5)
  102 format(' ','The curves correspond to the following depths: ',
     $           4(3X,A1,'-',I2,','),3X,A1,'-',I2,'.')
C     !END
      call BYE ('INOPOS')
C
      return
      end
