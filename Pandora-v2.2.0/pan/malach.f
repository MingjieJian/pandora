      subroutine MALACH
     $(NO,K)
C
C     Rudolf Loeser, 1988 Jun 01
C---- Prints headings, for COP.
C     (This is version 2 of MALACH.)
C     !DASH
      save
C     !DASH
      integer K, NO
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, LINER, HI, BYE
C
      call HI ('MALACH')
C     !BEG
      if((K.lt.1).or.(K.gt.4)) then
        write (MSSLIN(1),200) K
  200   format('K =',I12,', which is not .gt. 0 or .lt. 5.')
        call HALT ('MALACH',1)
      end if
C
      call LINER  (2,NO)
  201 format(' ',20X,A28,8('*********'),'*******')
C
      goto (101, 102, 103, 104), K
  101 continue
        write (NO,201) 'P R I N T O U T   **********'
        goto 100
  102 continue
        write (NO,201) 'C A L C U L A T I O N   ****'
        goto 100
  103 continue
        write (NO,201) 'M I S C E L L A N E O U S   '
        goto 100
  104 continue
        write (NO,201) 'T E S T / D E B U G   ******'
        goto 100
  100 continue
C
      call LINER  (2,NO)
C     !END
      call BYE ('MALACH')
C
      return
      end
