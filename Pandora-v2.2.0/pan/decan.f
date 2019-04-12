      subroutine DECAN
     $(KFUNC,I,DL,WVNUM,TC,FHZ,FAN,MUX,MYX,YY,KODE,TF,SF,SPHERE)
C
C     Rudolf Loeser, 1981 May 05
C---- Writes profile data to spectrum save file.
C     !DASH
      save
C     !DASH
      real*8 DL, FAN, FHZ, SF, TC, TF, WVNUM, YY
      integer I, KFUNC, KODE, LUSO, MUX, MYX
      logical SPHERE
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(28),LUSO )
C     !DASH
      external HI, BYE
C
      call HI ('DECAN')
C     !BEG
      if(KFUNC.eq.1) then
        write (LUSO,100)   I,DL,WVNUM,TC,FHZ,FAN, YY,MUX,MYX,KODE
  100   format(I10,1P5E20.12,0PF7.4,3I5)
      else
        if(SPHERE) then
          write (LUSO,101) I,DL,WVNUM,TC,FHZ,FAN, TF,SF
  101     format(I10,1P4E20.12,5E16.8)
        else
          write (LUSO,101) I,DL,WVNUM,TC,FHZ,FAN
        end if
      end if
C     !END
      call BYE ('DECAN')
C
      return
      end
