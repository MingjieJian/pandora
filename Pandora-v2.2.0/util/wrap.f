      program wrap
C
C     Rudolf Loeser, 2002 Apr 10
C
C---- Sets aside a PANDORA input file (---.dat) by modifying
C     the header, and the IOMX statement
C     !BDECL
      integer NI,NO,IGO
      character LINE*80
C     !EDECL
C     !DASH
      external modify_header
C
      data NI,NO /61, 62/
C     !beg
      rewind NO
C
      rewind NI
      read (NI,100) LINE
  100 format(A80)
      call modify_header (LINE(:65))
      write (NO,100) LINE
C
      IGO = 0
  101 continue
C
        read (NI,100) LINE
C
        if((LINE(:2).eq.'GO').or.(LINE(:2).eq.'go')) then
          IGO = IGO+1
        end if
        if(IGO.eq.1) then
          if((LINE(:4).eq.'IOMX').or.(LINE(:4).eq.'iomx')) then
            LINE = '> '//LINE
          end if
        end if
C
        write (NO,100) LINE
C
      if(IGO.lt.4) goto 101
C
      stop 'wrap 1.2: done'
C     !end
      end
      subroutine modify_header
     $(HEAD)
C
C     Rudolf Loeser, 2002 Apr 10
C          corrected 2006 Aug 29, 2006 Nov 16
C     !DASH
      save
C     !BDECL
      integer IT,JT,KT
      character HEAD*65
C     !EDECL
C     !DASH
      intrinsic mod
C     !beg
      if(HEAD(47:47).eq.'(') then
        read (HEAD(45:46),100) IT
  100   format(I2)
        KT = mod((IT+1),100)
      else
        read (HEAD(45:49),101) IT,JT
  101   format(I2,1X,I2)
        if(HEAD(47:47).eq.' ') then
          KT = mod((IT+1),100)
        else
          KT = mod((JT+1),100)
        end if
      end if
C
      write (HEAD(45:52),102) KT
  102 format(I2,'-??   ')
      if(HEAD(45:45).eq.' ') then
        HEAD(45:45) = '0'
      end if
C
      HEAD(53:65) = 'yyyy-mmm-dd  '
C
      print 103, HEAD
  103 format(' Header-in-waiting: ',A)
C     !end
      return
      end
