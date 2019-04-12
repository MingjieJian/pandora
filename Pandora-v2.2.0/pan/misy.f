      subroutine MISY
     $(LU,JNEDP)
C
C     Rudolf Loeser, 2003 Jun 27
C---- Prints editing explanation for the MYTU package.
C     !DASH
      save
C     !DASH
      integer JNEDP, LU
C     !DASH
      external HI, BYE
C
      call HI ('MISY')
C     !BEG
      if(LU.gt.0) then
        if(JNEDP.gt.0) then
          write (LU,100)
  100     format(' ','   Since the switch JNEDP = 1, a detailed dump ',
     $               'of this calculation was printed above.')
        else
          write (LU,101)
  101     format(' ','   To see a detailed dump of this calculation, ',
     $               'set the switch JNEDP = 1.')
        end if
      end if
C     !END
      call BYE ('MISY')
C
      return
      end
