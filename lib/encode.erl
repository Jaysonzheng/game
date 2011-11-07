%% Author: xiaoshengaya
%% Created: 2010-1-23
%% Description: 加密数据
-module(encode).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([encode/1]).

%%
%% API Functions
%%
encode(0) -> 16#70;
encode(1) -> 16#2f;
encode(2) -> 16#40;
encode(3) -> 16#5f;
encode(4) -> 16#44;
encode(5) -> 16#8e;
encode(6) -> 16#6e;
encode(7) -> 16#45;
encode(8) -> 16#7e;
encode(9) -> 16#ab;
encode(10) -> 16#2c;
encode(11) -> 16#1f;
encode(12) -> 16#b4;
encode(13) -> 16#ac;
encode(14) -> 16#9d;
encode(15) -> 16#91;
encode(16) -> 16#d;
encode(17) -> 16#36;
encode(18) -> 16#9b;
encode(19) -> 16#b;
encode(20) -> 16#d4;
encode(21) -> 16#c4;
encode(22) -> 16#39;
encode(23) -> 16#74;
encode(24) -> 16#bf;
encode(25) -> 16#23;
encode(26) -> 16#16;
encode(27) -> 16#14;
encode(28) -> 16#6;
encode(29) -> 16#eb;
encode(30) -> 16#4;
encode(31) -> 16#3e;
encode(32) -> 16#12;
encode(33) -> 16#5c;
encode(34) -> 16#8b;
encode(35) -> 16#bc;
encode(36) -> 16#61;
encode(37) -> 16#63;
encode(38) -> 16#f6;
encode(39) -> 16#a5;
encode(40) -> 16#e1;
encode(41) -> 16#65;
encode(42) -> 16#d8;
encode(43) -> 16#f5;
encode(44) -> 16#5a;
encode(45) -> 16#7;
encode(46) -> 16#f0;
encode(47) -> 16#13;
encode(48) -> 16#f2;
encode(49) -> 16#20;
encode(50) -> 16#6b;
encode(51) -> 16#4a;
encode(52) -> 16#24;
encode(53) -> 16#59;
encode(54) -> 16#89;
encode(55) -> 16#64;
encode(56) -> 16#d7;
encode(57) -> 16#42;
encode(58) -> 16#6a;
encode(59) -> 16#5e;
encode(60) -> 16#3d;
encode(61) -> 16#a;
encode(62) -> 16#77;
encode(63) -> 16#e0;
encode(64) -> 16#80;
encode(65) -> 16#27;
encode(66) -> 16#b8;
encode(67) -> 16#c5;
encode(68) -> 16#8c;
encode(69) -> 16#e;
encode(70) -> 16#fa;
encode(71) -> 16#8a;
encode(72) -> 16#d5;
encode(73) -> 16#29;
encode(74) -> 16#56;
encode(75) -> 16#57;
encode(76) -> 16#6c;
encode(77) -> 16#53;
encode(78) -> 16#67;
encode(79) -> 16#41;
encode(80) -> 16#e8;
encode(81) -> 16#0;
encode(82) -> 16#1a;
encode(83) -> 16#ce;
encode(84) -> 16#86;
encode(85) -> 16#83;
encode(86) -> 16#b0;
encode(87) -> 16#22;
encode(88) -> 16#28;
encode(89) -> 16#4d;
encode(90) -> 16#3f;
encode(91) -> 16#26;
encode(92) -> 16#46;
encode(93) -> 16#4f;
encode(94) -> 16#6f;
encode(95) -> 16#2b;
encode(96) -> 16#72;
encode(97) -> 16#3a;
encode(98) -> 16#f1;
encode(99) -> 16#8d;
encode(100) -> 16#97;
encode(101) -> 16#95;
encode(102) -> 16#49;
encode(103) -> 16#84;
encode(104) -> 16#e5;
encode(105) -> 16#e3;
encode(106) -> 16#79;
encode(107) -> 16#8f;
encode(108) -> 16#51;
encode(109) -> 16#10;
encode(110) -> 16#a8;
encode(111) -> 16#82;
encode(112) -> 16#c6;
encode(113) -> 16#dd;
encode(114) -> 16#ff;
encode(115) -> 16#fc;
encode(116) -> 16#e4;
encode(117) -> 16#cf;
encode(118) -> 16#b3;
encode(119) -> 16#9;
encode(120) -> 16#5d;
encode(121) -> 16#ea;
encode(122) -> 16#9c;
encode(123) -> 16#34;
encode(124) -> 16#f9;
encode(125) -> 16#17;
encode(126) -> 16#9f;
encode(127) -> 16#da;
encode(128) -> 16#87;
encode(129) -> 16#f8;
encode(130) -> 16#15;
encode(131) -> 16#5;
encode(132) -> 16#3c;
encode(133) -> 16#d3;
encode(134) -> 16#a4;
encode(135) -> 16#85;
encode(136) -> 16#2e;
encode(137) -> 16#fb;
encode(138) -> 16#ee;
encode(139) -> 16#47;
encode(140) -> 16#3b;
encode(141) -> 16#ef;
encode(142) -> 16#37;
encode(143) -> 16#7f;
encode(144) -> 16#93;
encode(145) -> 16#af;
encode(146) -> 16#69;
encode(147) -> 16#c;
encode(148) -> 16#71;
encode(149) -> 16#31;
encode(150) -> 16#de;
encode(151) -> 16#21;
encode(152) -> 16#75;
encode(153) -> 16#a0;
encode(154) -> 16#aa;
encode(155) -> 16#ba;
encode(156) -> 16#7c;
encode(157) -> 16#38;
encode(158) -> 16#2;
encode(159) -> 16#b7;
encode(160) -> 16#81;
encode(161) -> 16#1;
encode(162) -> 16#fd;
encode(163) -> 16#e7;
encode(164) -> 16#1d;
encode(165) -> 16#cc;
encode(166) -> 16#cd;
encode(167) -> 16#bd;
encode(168) -> 16#1b;
encode(169) -> 16#7a;
encode(170) -> 16#2a;
encode(171) -> 16#ad;
encode(172) -> 16#66;
encode(173) -> 16#be;
encode(174) -> 16#55;
encode(175) -> 16#33;
encode(176) -> 16#3;
encode(177) -> 16#db;
encode(178) -> 16#88;
encode(179) -> 16#b2;
encode(180) -> 16#1e;
encode(181) -> 16#4e;
encode(182) -> 16#b9;
encode(183) -> 16#e6;
encode(184) -> 16#c2;
encode(185) -> 16#f7;
encode(186) -> 16#cb;
encode(187) -> 16#7d;
encode(188) -> 16#c9;
encode(189) -> 16#62;
encode(190) -> 16#c3;
encode(191) -> 16#a6;
encode(192) -> 16#dc;
encode(193) -> 16#a7;
encode(194) -> 16#50;
encode(195) -> 16#b5;
encode(196) -> 16#4b;
encode(197) -> 16#94;
encode(198) -> 16#c0;
encode(199) -> 16#92;
encode(200) -> 16#4c;
encode(201) -> 16#11;
encode(202) -> 16#5b;
encode(203) -> 16#78;
encode(204) -> 16#d9;
encode(205) -> 16#b1;
encode(206) -> 16#ed;
encode(207) -> 16#19;
encode(208) -> 16#e9;
encode(209) -> 16#a1;
encode(210) -> 16#1c;
encode(211) -> 16#b6;
encode(212) -> 16#32;
encode(213) -> 16#99;
encode(214) -> 16#a3;
encode(215) -> 16#76;
encode(216) -> 16#9e;
encode(217) -> 16#7b;
encode(218) -> 16#6d;
encode(219) -> 16#9a;
encode(220) -> 16#30;
encode(221) -> 16#d6;
encode(222) -> 16#a9;
encode(223) -> 16#25;
encode(224) -> 16#c7;
encode(225) -> 16#ae;
encode(226) -> 16#96;
encode(227) -> 16#35;
encode(228) -> 16#d0;
encode(229) -> 16#bb;
encode(230) -> 16#d2;
encode(231) -> 16#c8;
encode(232) -> 16#a2;
encode(233) -> 16#8;
encode(234) -> 16#f3;
encode(235) -> 16#d1;
encode(236) -> 16#73;
encode(237) -> 16#f4;
encode(238) -> 16#48;
encode(239) -> 16#2d;
encode(240) -> 16#90;
encode(241) -> 16#ca;
encode(242) -> 16#e2;
encode(243) -> 16#58;
encode(244) -> 16#c1;
encode(245) -> 16#18;
encode(246) -> 16#52;
encode(247) -> 16#fe;
encode(248) -> 16#df;
encode(249) -> 16#68;
encode(250) -> 16#98;
encode(251) -> 16#54;
encode(252) -> 16#ec;
encode(253) -> 16#60;
encode(254) -> 16#43;
encode(255) -> 16#f.

%%
%% Local Functions
%%
