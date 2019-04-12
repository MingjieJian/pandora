      subroutine DAMEE
     $(NLTE,ABDION,PF,SUM,SA,SO,XNK)
C
C     Rudolf Loeser, 1999 Jan 04
C---- Prints details for JASON.
C     !DASH
      save
C     !DASH
      real*8 ABDION, PF, SA, SO, SUM, XNK
      integer I, LUEO, N, NLTE
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, HI, BYE
C
C               ABDION(N), SUM(N), SA(N), SO(N), XNK(N), PF(N)
      dimension ABDION(*), SUM(*), SA(*), SO(*), XNK(*), PF(*)
C
      call HI ('DAMEE')
C     !BEG
      if(NLTE.eq.1) then
        write (LUEO,100) 'non-LTE'
  100   format(' ','Details of the calculation of ',A,' values of NK'//
     $         ' ',32X,'SUM',13X,'PF',13X,'SA',9X,'ABDION',28X,'SO',
     $             13X,'NK')
      else
        write (LUEO,100) 'LTE'
      end if
      call LINER  (1, LUEO)
C
      write (LUEO,101) (I,SUM(I),PF(I),SA(I),ABDION(I),SO(I),XNK(I),
     $                  I=1,N)
  101 format(5(' ',I5,15X,1P4E15.7,15X,2E15.7/))
C     !END
      call BYE ('DAMEE')
C
      return
      end
