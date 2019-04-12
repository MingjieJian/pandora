      subroutine ADVENT
     $(KILROY,NO,N,KNT,XW,XJBAR,BJL,B,XL,XMBJL,XHBJL)
C
C     Rudolf Loeser, 1980 Feb 19
C---- Makes detailed printouts, for SALINA.
C     (This is version 2 of ADVENT.)
C     !DASH
      save
C     !DASH
      real*8 B, BJL, XHBJL, XJBAR, XL, XMBJL, XW
      integer I, J, KARA, KNT, N, NO
      logical KILROY
      character BLANK*1, LINE*128
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external ABJECT, LINER, HI, BYE
C
C               XHBJL(N,2), XJBAR(N,2), XMBJL(N,2), XL(N,2), BJL(N,2),
      dimension XHBJL(N,*), XJBAR(N,*), XMBJL(N,*), XL(N,*), BJL(N,*),
C
C               XW(2), B(N,2)
     $          XW(*), B(N,*)
C
      call HI ('ADVENT')
C     !BEG
      if(NO.gt.0) then
        if(KILROY) then
          KILROY = .false.
          call ABJECT (NO)
          write (NO,100)
  100     format(' ','Details of Cooling Rates calculation for H- ',
     $               'free-free and H free-free.')
        end if
        call LINER    (1,NO)
C     !EJECT
        LINE = BLANK
        KARA = -64
        do 102 J = 1,KNT
          KARA = KARA+64
          write (LINE(KARA+24:KARA+40),101) XW(J)
  101     format('LM=',1PE14.8)
  102   continue
        write (NO,103) LINE
  103   format(A128)
C
        LINE = BLANK
        KARA = -64
        do 105 J = 1,KNT
          KARA = KARA+64
          write (LINE(KARA+1:KARA+64),104)
  104     format(13X,'J',9X,'B',9X,'L',7X,'BJL',6X,'MBJL',6X,'HBJL')
  105   continue
        write (NO,103) LINE
C
        do 108 I = 1,N
C
          LINE = BLANK
          KARA = -64
          do 107 J = 1,KNT
            KARA = KARA+64
            write (LINE(KARA+1:KARA+64),106) I,
     $             XJBAR(I,J),B(I,J),XL(I,J),BJL(I,J),XMBJL(I,J),
     $             XHBJL(I,J)
  106       format(I4,1P6E10.2)
  107     continue
          write (NO,103) LINE
C
  108   continue
C
      end if
C     !END
      call BYE ('ADVENT')
C
      return
      end
