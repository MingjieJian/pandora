      subroutine MODRON
     $(LU,KODE,N,NL,CHT,CHL,SUM)
C
C     Rudolf Loeser, 1982 Feb 24
C---- Saves heating rates data for plotting.
C     !DASH
      save
C     !DASH
      real*8 CHL, CHT, SUM
      integer IL, IU, IUL, KODE, LU, MODE, N, NL, NOION
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
      equivalence (KZQ( 94),NOION)
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external INDXUL, BUNT, PANT, PENT, HALT, HI, BYE
C
C               CHT(N,NT), CHL(N,NL)
      dimension CHT(N,*),  CHL(*)
C
      data MODE /1/
C     !EJECT
C
      call HI ('MODRON')
C     !BEG
      if((LU.gt.0).and.(NOION.le.0)) then
C
        if(KODE.eq.1) then
          write (LU,100)
  100     format('[   H E A T I N G   ] > ')
        else if(KODE.eq.2) then
          write (LU,101)
  101     format('[   Integrated  H E A T I N G   ] > ')
        else
          write (MSSLIN(1),102) KODE
  102     format('KODE =',I12,', which is neither 1 nor 2.')
          call HALT     ('MODRON',1)
        end if
C
        write (LU,103)
  103   format('[   CHT = transitions   ] > ')
        do 105 IU = 2,NL
          do 104 IL = 1,(IU-1)
            call INDXUL (IU,IL,IUL)
            call PENT   (LU,CHT(1,IUL),IU,IL,'CHT')
  104     continue
  105   continue
C
        write (LU,106)
  106   format('[   CHL = transitions to continuum   ] > ')
        call PANT       (LU,CHL,N,NL,MODE,'CHL')
C
        write (LU,107)
  107   format('[   SUM = total rate   ] > ')
        call BUNT       (LU,SUM,'SUM')
C
      end if
C     !END
      call BYE ('MODRON')
C
      return
      end
