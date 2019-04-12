      subroutine EDGAR
     $(TE,POPMSS,RM,POPSYM,NPOPS,VION,DMPI)
C
C     Rudolf Loeser, 1990 Oct 03
C---- Computes ion velocities, for ion broadening calculation.
C     (This is version 2 of EDGAR.)
C     !DASH
      save
C     !DASH
      real*8 CON8, POPMSS, RM, TE, TERM, VION
      integer J, LUEO, NPOPS
      logical DMPI
      character POPSYM*3
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external RIGEL, ZERO1, LINER, HI, BYE
C
C               RM(NPOPS), POPMSS(NPOPS), VION(NPOPS), POPSYM(NPOPS)
      dimension RM(*),     POPMSS(*),     VION(*),     POPSYM(*)
C
      call HI ('EDGAR')
C     !BEG
      call ZERO1   (VION, NPOPS)
C
      if(DMPI) then
        call LINER (1, LUEO)
        write (LUEO,100)
  100   format(' ','Mass, mass-ratio, and velocity for:')
      end if
C
      call RIGEL   (8, CON8)
      TERM = TE*CON8
      do 102 J = 1,NPOPS
        if((J.ne.1).and.(J.ne.5)) then
          VION(J) = sqrt(TERM/RM(J))
          if(DMPI) then
            write (LUEO,101) POPSYM(J),POPMSS(J),RM(J),VION(J)
  101       format(' ',A3,1P3E14.6)
          end if
        end if
  102 continue
C
      if(DMPI) then
        call LINER (1, LUEO)
        write (LUEO,103) TERM
  103   format(' ','Constant term =',1PE14.6)
      end if
C     !END
      call BYE ('EDGAR')
C
      return
      end
