      subroutine VERNAL
     $(N,IU,IL,XNU,SET,SR,B,S)
C
C     Rudolf Loeser, 1990 Jul 27
C---- Dumps, for SADIE.
C     !DASH
      save
C     !DASH
      real*8 B, DNU, S, SET, SR, XNU
      integer I, IL, ISNDD, IU, IUL, LUEO, MO, N
      character MARK*1
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(113),ISNDD)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external LINER, BAYBARS, SHIM, MESHED, MASHED, INDXUL, HI, BYE
C
C               XNU(NSL), S(N), B(N), SR(N), SET(N,MUL)
      dimension XNU(*),   S(*), B(*), SR(*), SET(N,*)
C     !EJECT
C
      call HI ('VERNAL')
C     !BEG
      if((MO.gt.0).and.(ISNDD.gt.0)) then
C
        call MESHED    ('VERNAL', 2)
        DNU = XNU(IU)-XNU(IL)
        write (LUEO,100) IU,IL,ISNDD,IL,XNU(IL),IU,XNU(IU),DNU
  100   format(' ','***** Details of direct calculation of S(',I2,
     $             '/',I2,')'/
     $         ' ','(This output is controlled by input parameter ',
     $             'ISNDD; in this run ISNDD =',I2,'.)'/
     $         ' ','nu(',I2,') =',F14.8,', nu(',I2,') =',F14.8,
     $             ', dnu =',F14.8//
     $         ' ',18X,'SET',15X,'B',11X,'S-raw',9X,'S-smooth')
        call LINER     (1, LUEO)
C
        call INDXUL    (IU, IL, IUL)
        do 102 I = 1,N
          call BAYBARS (SR(I),S(I),MARK)
          write (LUEO,101) I,SET(I,IUL),B(I),SR(I),MARK,S(I)
  101     format(' ',I5,1P3E16.8,A1,E16.8)
          call SHIM    (I, 5, LUEO)
  102   continue
C
        call MASHED    ('VERNAL')
C
      end if
C     !END
      call BYE ('VERNAL')
C
      return
      end
