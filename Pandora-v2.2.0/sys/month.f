      subroutine MONTH
     $(KODE,NAME,NUM,ERROR)
C     Rudolf Loeser, 1986 May 08
C---- Converts   name-of-month to number-of-month (KODE=1), or
C     converts number-of-month to   name-of-month (KODE=2).
C---- Returns ERROR=.false. if OK, = .true. if something is wrong.
C     !DASH
      save
C     !DASH
      integer I, KODE, NUM
      logical ERROR
      character NAME*3, TABL*3, TABU*3
C     !DASH
      dimension TABU(12), TABL(12)
C
      data      TABU /'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
     $                'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/
      data      TABL /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     $                'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/
C
C     !BEG
      ERROR = .true.
      if(KODE.eq.1) then
        do 100 I=1,12
          if((NAME.eq.TABU(I)).or.(NAME.eq.TABL(I))) then
            NUM   = I
            ERROR = .false.
            goto 101
          end if
  100   continue
  101   continue
      else if(KODE.eq.2) then
        if((NUM.ge.1).and.(NUM.le.12)) then
          NAME  = TABL(NUM)
          ERROR = .false.
        end if
      end if
C     !END
C
      return
      end
