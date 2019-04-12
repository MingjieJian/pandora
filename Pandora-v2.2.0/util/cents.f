      program cents
C
C     Rudolf Loeser, 2002 Apr 08
C     Add Oxygen, 2004 Mar 04
C     Add Sulphur (soon after)
C     Twiddle to handle less than MXION files, 2005 May 26
C     New PANDORA output line format, 2006 Aug 25
C     Choose TARGET, 2007 Mar 20
C
C     SGK Dec 18 2013/Mar 19 2014
C     moved list of ions to a 'ions.list' file (in cwd), 
C     read with subroutine get_ionslist(), only limit is MATX=??
C
C---- Constructs an input file for the CENSUS program,
C     in the special case of: routine updating of a given atom.
C
c uses following routines in sys/ 
c      run_data is_digit get_date get_time timeday

      real*8 FILESD
      integer NI,NX,NO,MXION,MAT,IAT,ILF,NCHAR,MXF,MLS,NLS
      integer LNFNAM,I,J,NLZ,LCOMB,NION,LDAT
      character ATNAME*2,MODNAME*8,ATTAB*2,FILENM*60,FION*80
      character TARGET*6,HEAD*80,PARA*80,Z*80,ZZ*80,FLVS*80
      character COMBO*40
C
      external get_names, get_files, get_data
C
      parameter (MLS=100)
      dimension Z(MLS), ZZ(MLS), FLVS(MLS), FION(MLS)
C
      parameter (MXF=9)
      dimension FILENM(MXF), FILESD(MXF),LNFNAM(MXF)
      data FILENM /MXF*' '/
      data LNFNAM /MXF*0/
      data FILESD /MXF*0.D0/
C
      parameter (MATX=20)
      dimension ATTAB(MATX), NCHAR(MATX), MXION(MATX)
C
      data LU, NI,NX,NO /60, 61, 62, 63/
c
      call get_ionslist(LU, ATTAB, NCHAR, MXION, MAT, MATX)
C
      rewind NI
      call get_names (NI,ATNAME,MODNAME,IAT,ATTAB,NCHAR,MAT,
     $                COMBO,LCOMB)
C
      call get_files (NI,COMBO,LCOMB,FILENM,LNFNAM,FILESD,ILF,
     $                MXION(IAT),NX,NION)
C
      TARGET = 'CENSUS'
      if(ATNAME.eq.'he') then
        print 100
  100   format(' Type 1 to use regular data, 2 to use diffusion ',
     $         'data (I1): ',$)
        accept 101, LDAT
  101   format(I1)
        if(LDAT.eq.2) then
          TARGET = 'CENSOR'
        end if
      end if
C
      call get_data (NX,FILENM(ILF),LNFNAM(ILF),TARGET,HEAD,PARA,
     $               Z,FION,FLVS,NLZ,MLS)
      print 102, TARGET,NLZ,MLS,NION
  102 format(' ','TARGET = ',A6,'; NLZ =',I3,', max = ',I3,
     $           ', # of ion files = ',I3/
     $       ' ','Common Z:')
      print 103, (Z(I),I=1,NLZ)
  103 format(' ',A80)
C
      rewind NO
      do 105 J = 1,NION
        call get_data (NX,FILENM(J),LNFNAM(J),TARGET,HEAD,PARA,
     $                 ZZ,FION,FLVS,NLS,MLS)
        if(NLS.ne.NLZ) stop 'cents: NLS .ne. NLZ'
  104   format(A80)
        write (NO,104) HEAD
        write (NO,104) PARA
        write (NO,104) (Z(I),I=1,NLZ)
        write (NO,104) (FLVS(I),I=1,NLS)
        write (NO,104) (FION(I),I=1,NLS)
  105 continue
C
      stop 'cents v4.02: done'
C
      end
C
C ---------------------------------------------------------------------------
C
      subroutine get_ionslist(LU, ATTAB, NCHAR, MXION, MAT, MATX)
c
      character line*40
      character ATTAB*(*)
      dimension ATTAB(MATX), NCHAR(MATX), MXION(MATX)
c     
      open(lu, file='ions.list', status='OLD', readonly)
      mat = 0
 10   read(lu,'(a40)',end=100) line
      mat = mat + 1
      do k =2, len(line)
        if (line(k:k).eq.'.') then
          nchar(mat) = k-1
          attab(mat) = line(:nchar(mat))
          read(line(k+1:), *) n
          mxion(mat) = n
        end if
      end do
      if (mat.lt.matx) goto 10
 100  close(lu)
c
      print 1001, mat, ' ions in ions.list (max=',matx,')'
c
cD      do 101 i = 1, mat
cD 101    print 1002, attab(i)(:nchar(i)), ':', mxion(i), ' lvls'
c
 1001 format(x,i3,a,i3,a)
 1002 format(x,a3,a,i3,a)
c
      end
C
C ---------------------------------------------------------------------------
C
      subroutine get_names
     $(NI,ATNAME,MODNAME,IAT,ATTAB,NCHAR,MAT,COMBO,LCOMB)
C
C     Rudolf Loeser, 2002 Apr 08
C
      save
C
      integer NI,IAT,MAT,IB,J,IA,I,IS,NCHAR,LCOMB
      character ATTAB*(*),ATNAME*(*),MODNAME*(*),LINE*20
      character COMBO*(*)
C
      intrinsic index
C
      dimension ATTAB(*), NCHAR(*)
C
      read (NI,100) LINE
  100 format(A)
      IB = index(LINE,' ')
      IS = IB-1
C
      do 102 J = 2,1,-1
        IA = IS-J+1
        do 101 I = 1,MAT
          if(NCHAR(I).eq.J) then
            if(LINE(IA:IS).eq.ATTAB(I)(:J)) then
              ATNAME = ATTAB(I)(:J)
              IAT = I
              MODNAME = LINE(:(IA-1))
              goto 104
            end if
          end if
  101   continue
  102 continue
      print 103, IA,IS,IB,LINE
  103 format(' ',3I3,' [',A,']')
      stop 'get_names: failed'
C
  104 continue
      COMBO = LINE(:IS)
      LCOMB = IS
      print 105, MODNAME,ATNAME,LCOMB,IAT
  105 format(' ','MODNAME = ',A,', ATNAME = ',A,10X,2I4)
C
      return
      end
C
C ---------------------------------------------------------------------------
C
      subroutine get_files
     $(NI,COMBO,LCOMB,FILENM,LNFNAM,FILESD,ILF,MXF,NX,NION)
C
C     Rudolf Loeser, 2002 Apr 08, 2005 May 26
C
      save
C
      real*8 FILESD,SD,SDMAX
      integer NI,ILF,MXF,IB,II,IX,ION,NX,LN,I,LNFNAM
      integer LCOMB,IC,JL,NION
      character FILENM*(*),LINE*60,MARK*1,COMBO*(*)
      logical STOP,YES
C
      external  get_timestamp, IS_DIGIT
      intrinsic index
C
      dimension FILENM(*), FILESD(*), LNFNAM(*)
C
      SDMAX = FILESD(1)
      ILF = 1
C
  100 continue
        read (NI,101,end=103) LINE
  101   format(A)
C
        IX = index (LINE,'.msc.')
        II = IX-1
C
        IC = index (LINE,COMBO(:LCOMB))
        JL = IC+(LCOMB-1)
        if((II-JL).ne.1) then
          goto 100
        end if
        call IS_DIGIT (LINE(II:II), YES)
        if(.not.YES) goto 100
C
        IB = index (LINE,' ')
        LN = IB-1
C
        read (LINE(II:II),102) ION
  102   format(I1)
        if(ION.gt.MXF) stop 'get_files: ION # too big'
C
        call get_timestamp (LINE,LN,SD,NX)
        if(SD.gt.FILESD(ION)) then
          FILENM(ION) = LINE(:LN)
          LNFNAM(ION) = LN
          FILESD(ION) = SD
        end if
        if(SD.gt.SDMAX) then
          SDMAX = SD
          ILF = ION
        end if
      goto 100
C
  103 continue
C
      NION = 0
      do 105 I = 1,MXF
        STOP = FILENM(I).eq.' '
        if(.not.STOP) then
          NION = NION+1
        end if
        MARK = ' '
        if(I.eq.ILF) then
          MARK = '*'
        end if
        print 104, FILENM(I),FILESD(I),MARK
  104   format(' ',A,3X,F12.5,A1)
  105 continue
      if(STOP.and.(NION.lt.2)) stop 'get_files: missing file'
C
      return
      end
C
C ---------------------------------------------------------------------------
C
      subroutine get_timestamp
     $(NAME,LN,SD,LU)
C
C     Rudolf Loeser, 2002 Apr 09
C
      save
C
      real*8 SD
      integer LN,LU,jummy
      character NAME*(*),qummy*8
C
      external RUN_DATA
C
      close (LU)
      open (unit=LU, file=NAME(:LN), status='OLD', readonly)
      call RUN_DATA (LU,2,jummy,jummy,jummy,jummy,jummy,qummy,jummy,
     $               qummy,qummy,SD)
C
      return
      end
C
C ---------------------------------------------------------------------------
C
      subroutine get_data
     $(LU,FILENM,LN,TARGET,HEAD,PARA,Z,FION,FLVS,NLS,MAX)
C
C     Rudolf Loeser, 2002 Apr 09
C
      save
C
      integer LU,LN,NLS,MAX,I,K
      character TARGET*6,HEAD*80,PARA*80,Z*80,FION*80,FLVS*80,
     $          FILENM*(*),LINE*80
C
      intrinsic index
C
      dimension Z(*), FION(*), FLVS(*)
C
      close (LU)
      open (unit=LU, file=FILENM(:LN), status='OLD', readonly)
      rewind (LU)
C
  100 continue
        read (LU,101) LINE
  101   format(A80)
        if(LINE(3:8).ne.TARGET) then
          goto 100
        end if
      read (LU,101) HEAD
      read (LU,101) PARA
C
      NLS = 0
  102 continue
        NLS = NLS+1
        if(NLS.gt.MAX) stop 'get_data: too many data lines'
        read (LU,101) Z(NLS)
        K = index(Z(NLS),') >')
        if(K.eq.0) then
          goto 102
        end if
C
      read (LU,101) (FLVS(I),I=1,NLS)
      read (LU,101) (FION(I),I=1,NLS)
C
      read (LU,101) LINE
      if(LINE(3:8).ne.TARGET) then
        print *, 'get_data: TARGET end error:'
        print *, '  <'//LINE(3:8)//'> .ne. <'//TARGET//'>'
        print *, ' *** check that the sections between'
        print *, '   [ '//TARGET//' data start ]'
        print *, ' and '
        print *, '   [ '//TARGET//' data end ]'
        print *, ' are consistent in the msc files ***'
        stop 'get_data: TARGET end error'
      endif 
C
      return
      end
