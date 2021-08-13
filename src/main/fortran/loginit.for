c loginit - initialize logging common block values
c_________________________________________________________________NoticeStart_
c StateMod Water Allocation Model
c StateMod is a part of Colorado's Decision Support Systems (CDSS)
c Copyright (C) 1994-2021 Colorado Department of Natural Resources
c
c StateMod is free software:  you can redistribute it and/or modify
c     it under the terms of the GNU General Public License as published by
c     the Free Software Foundation, either version 3 of the License, or
c     (at your option) any later version.
c
c StateMod is distributed in the hope that it will be useful,
c     but WITHOUT ANY WARRANTY; without even the implied warranty of
c     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c     GNU General Public License for more details.
c
c     You should have received a copy of the GNU General Public License
c     along with StateMod.  If not, see <https://www.gnu.org/licenses/>.
c_________________________________________________________________NoticeEnd___

      subroutine loginit()

      include 'common.inc'

      ! Initialize all logging common block variables to zero (meaning no logging).

      log_IOUT01=0
      log_IOUT02=0
      log_IOUT03=0
      log_IOUT04=0
      log_IOUT05=0
      log_IOUT06=0
      log_IOUT07=0
      log_IOUT09=0
      log_IOUT10=0
      log_IOUT11=0
      log_IOUT12=0
      log_IOUT13=0
      log_IOUT14=0
      log_IOUT15=0
      log_IOUT16=0
      log_IOUT17=0
      log_IOUT18=0
      log_IOUT19=0
      log_IOUT20=0
      log_IOUT21=0
      log_IOUT22=0
      log_IOUT23=0
      log_IOUT24=0
      log_IOUT25=0
      log_IOUT26=0
      log_IOUT27=0
      log_IOUT28=0
      log_IOUT29=0
      log_IOUT30=0
      log_IOUT31=0
      log_IOUT32=0
      log_IOUT33=0
      log_IOUT34=0
      log_IOUT35=0
      log_IOUT36=0
      log_IOUT37=0
      log_IOUT38=0
      log_IOUT39=0
      log_IOUT40=0
      log_IOUT41=0
      log_IOUT42=0
      log_IOUT43=0
      log_IOUT45=0
      log_IOUT45X=0
      log_IOUT46=0
      log_IOUT47=0
      log_IOUT48=0
      log_IOUT49=0
      log_IOUT50=0
      log_IOUT51=0
      log_IOUT52=0
      log_IOUT54=0
      log_IOUT6=0
      log_IOUT8=0
      log_IOUT=0
      log_IOUTA=0
      log_IOUTADJ=0
      log_IOUTB=0
      log_IOUTC=0
      log_IOUTCR=0
      log_IOUTCS=0
      log_IOUTCU=0
      log_IOUTCX=0
      log_IOUTD=0
      log_IOUTE=0
      log_IOUTEF=0
      log_IOUTEV=0
      log_IOUTF=0
      log_IOUTG1=0
      log_IOUTG=0
      log_IOUTGVC=0
      log_IOUTGX=0
      log_IOUTHG1=0
      log_IOUTHG=0
      log_IOUTI=0
      log_IOUTIN=0
      log_IOUTIR=0
      log_IOUTIW=0
      log_IOUTJM=0
      log_IOUTL=0
      log_IOUTLIM=0
      log_IOUTN=0
      log_IOUTNG=0
      log_IOUTOUT=0
      log_OUTP=0
      log_OUTP1=0
      log_IOUTPPT=0
      log_IOUTPRF=0
      log_IOUTPU=0
      log_IOUTPUC=0
      log_IOUTQ=0
      log_IOUTR=0
      log_IOUTRE2=0
      log_IOUTRE=0
      log_IOUTREP=0
      log_IOUTRF=0
      log_IOUTRGF=0
      log_IOUTRGS=0
      log_IOUTRO=0
      log_IOUTRTN=0
      log_IOUTS=0
      log_IOUTSEP=0
      log_IOUTSM=0
      log_IOUTSO=0
      log_IOUTSP=0
      log_IOUTT=0
      log_IOUTTAR=0
      log_IOUTURM=0
      log_IOUTW1=0
      log_IOUTW=0
      log_IOUTWR=0
      ! TODO smalers 2021-07-31 evaluate how to handle the following, which is sometimes an array, sometimes not.
      !log_IOUTX_array(IX)=0
      log_IOUTX=0
      log_IOUTY=0
      log_IOUTZ=0

      return
      end
