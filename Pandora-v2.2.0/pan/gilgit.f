      subroutine GILGIT
     $(NSHL,MRR,TOPT,TITLE,CALLER,DUMP)
C
C     Rudolf Loeser, 1981 Nov 03
C---- Sets up dump output, and prints header, for HELIOS.
C     !DASH
      save
C     !DASH
      integer IQWDD, K, LUEO, MO, MRR, NSHL
      logical DUMP, TOPT
      character CALLER*(*), TITLE*100, TYPE*9
C     !COM
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(146),IQWDD)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external MESHED, HI, BYE
C
      dimension TYPE(2)
C
      data TYPE /'Linear', 'Quadratic'/
C     !EJECT
C
      call HI ('GILGIT')
C     !BEG
      DUMP = (IQWDD.gt.0).and.(MO.gt.0)
      if(DUMP) then
        K = 1
        if(TOPT) then
          K = 2
        end if
C
        call MESHED (CALLER, 2)
        write (LUEO,100) TITLE,NSHL,MRR,TYPE(K)
  100   format(' ','Details of spherical weight matrices calculation ',
     $             'for:'/
     $         ' ',A100//
     $         ' ',I3,' Shell rays; ',I3,' Disk rays.',10X,
     $             'TAU integration: ',A,'.')
      end if
C     !END
      call BYE ('GILGIT')
C
      return
      end
