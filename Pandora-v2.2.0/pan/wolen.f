      subroutine WOLEN
     $(NLTE,J,ABDION,BDI,GMI,PF,SUM,SA,SO,XND)
C
C     Rudolf Loeser, 2000 Jan 03
C---- Prints details for KULLOCH.
C     !DASH
      save
C     !DASH
      real*8 ABDION, B, BDI, GMI, ONE, PF, SA, SO, SUM, XND
      integer I, J, LUEO, N, NLTE
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, SHIM, HI, BYE
C
C               ABDION(N), BDI(N,NL), GMI(N,NL), XND(N), SUM(N), SA(N),
      dimension ABDION(*), BDI(N,*),  GMI(N,*),  XND(*), SUM(*), SA(*),
C
C               SO(N), PF(N)
     $          SO(*), PF(*)
C     !EJECT
C
      call HI ('WOLEN')
C     !BEG
      call LINER  (3, LUEO)
      if(NLTE.eq.1) then
        write (LUEO,100) 'non-LTE',J,J
  100   format(' ','Details of the calculation of ',A,' values of ',
     $             'N(',I2,')'//
     $         ' ',19X,'B',12X,'SUM',13X,'PF',13X,'SA',9X,'ABDION',
     $             12X,'GMI',13X,'SO',10X,'N(',I2,')')
      else
        write (LUEO,100) 'LTE',J,J
      end if
      call LINER  (1, LUEO)
C
      do 102 I = 1,N
        if(NLTE.eq.1) then
          B = BDI(I,J)
        else
          B = ONE
        end if
        write (LUEO,101) I,B,SUM(I),PF(I),SA(I),ABDION(I),GMI(I,J),
     $                   SO(I),XND(I)
  101   format(' ',I5,1P8E15.7)
        call SHIM (I, 5, LUEO)
  102 continue
C     !END
      call BYE ('WOLEN')
C
      return
      end
