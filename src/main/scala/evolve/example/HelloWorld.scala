/*
  Copyright (c) 2016, Elliot Stirling
  All rights reserved.

  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:

  * Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.

  * Neither the name of the {organization} nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
  ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

package evolve.example

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.concurrent.Executors

import evolve.core.Evolver.EvolverStrategy
import evolve.core._

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, ExecutionContext, Future, blocking}


object HelloWorld {
  def main(args: Array[String]): Unit = {
    import evolve.functions.NeuralFunctions._

    implicit val ec = ExecutionContext.fromExecutor( Executors.newFixedThreadPool( Runtime.getRuntime.availableProcessors() ) )
    def ins( index: Int, a: Int, b: Int ): Instruction = {
      val instructionSize = functions(index).instructionSize
      val argumentSize = functions(index).argumentSize

      val ins = Instruction(0)
        .instruction( index, instructionSize )
        .pointer(a, instructionSize, argumentSize)
        .pointer(b, instructionSize + argumentSize, argumentSize)

      assert( ins.instruction( instructionSize ) == index )
      assert( ins.pointer( instructionSize, argumentSize ) == a )
      assert( ins.pointer( instructionSize + argumentSize, argumentSize ) == b )
      ins
    }

    def score( input: List[Double], output: Stream[Double] ): Double = {

      val score: Stream[Double] =
        input
          .toStream
          .zip(output)
          .map( a => scoreFunc(a._1, a._2).toDouble )

      val complete: Int = score.takeWhile( _ < Int.MaxValue * 0.4999 ).length

      val remainder: Stream[Double] = score.drop( complete )

      remainder match {
        case h1 #:: h2 #:: h3 #:: _   => h1 * 8.0 + h2 * 2.0 + h3 + (input.length - complete - 1) * (Long.MaxValue / 256L).toDouble
        case h1 #:: h2 #:: _          => h1 * 2.0 + h2 + (input.length - complete - 1) * (Long.MaxValue / 256L).toDouble
        case h1 #:: _                 => h1 + (input.length - complete) * (Long.MaxValue / 256L).toDouble
        case _                        => 0.0
      }
    }

    def trial( program: Program ): Stream[Double] = {
      def go( input: Double, arg0: Double, arg1: Double, memory: List[Double] ): Stream[Double] = {
        val res = program(List(input, arg0, arg1), memory)
        val out = res._1.result(program.outputCount).toList
        out.head #:: go(out.head, out(1), out(2), res._2)
      }

      go( 0.0, 0.0, 0.0, List.fill[Double](program.data.length)(0.0))
    }

    val string = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur? At vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui officia deserunt mollitia animi, id est laborum et dolorum fuga. Et harum quidem rerum facilis est et expedita distinctio. Nam libero tempore, cum soluta nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere possimus, omnis voluptas assumenda est, omnis dolor repellendus. Temporibus autem quibusdam et aut officiis debitis aut rerum necessitatibus saepe eveniet ut et voluptates repudiandae sint et molestiae non recusandae. Itaque earum rerum hic tenetur a sapiente delectus, ut aut reiciendis voluptatibus maiores alias consequatur aut perferendis doloribus asperiores repellat."
    val scorable_string = string.toList.map( _.toDouble + 0.5 )

    def run( iteration: Long, p: Program, es: EvolverStrategy ): Unit = Evolver(p, a => score(scorable_string, trial(a)), optimise = true)(es, functions, ec) match {

      case Some(program) if iteration % 1000 == 0 =>

          println( score(scorable_string, trial(program)) )
          println(trial(program).map( _.floor.toChar ).take(string.length*2).mkString)
          println(program.shrink)
          Files.write(Paths.get("solution.dot"), DotGraph(program).getBytes(StandardCharsets.UTF_8) )
          if( program.length < program.shrink.length * 8 )
            run(iteration + 1, program, es)
          else
            run(iteration + 1, program.shrink , es)


      case Some(program) => run(iteration + 1, program, es)
      case _             =>
        try {
          run( iteration + 1, p.spread(), es )
        } catch {
          case _: Throwable =>
            run( iteration + 1, p.shrink, es )
        }
    }

    val start = Program(6,ArrayBuffer(Instruction(135009335), Instruction(738205697), Instruction(1811955715), Instruction(8193), Instruction(23847), Instruction(30047), Instruction(872456192), Instruction(469762050), Instruction(469770249), Instruction(805371907), Instruction(402743299), Instruction(1476438811), Instruction(268468230), Instruction(268451840), Instruction(201449472), Instruction(1543643144), Instruction(1677746178), Instruction(1543528449), Instruction(1476539467), Instruction(31325), Instruction(1409304868), Instruction(939531386), Instruction(402808832), Instruction(106514116), Instruction(1812070420), Instruction(402841616), Instruction(805306375), Instruction(402661379), Instruction(1745059860), Instruction(1476532020), Instruction(268615698), Instruction(115831571), Instruction(335568899), Instruction(1744846864), Instruction(469762050), Instruction(1745043456), Instruction(402890766), Instruction(939532341), Instruction(1006825590), Instruction(1543692313), Instruction(939749873), Instruction(67125266), Instruction(671195180), Instruction(201490452), Instruction(402931757), Instruction(537028465), Instruction(1812111407), Instruction(536945930), Instruction(1006797345), Instruction(268632097), Instruction(266885), Instruction(335675443), Instruction(469925900), Instruction(1543512065), Instruction(805535796), Instruction(80086882), Instruction(939794399), Instruction(940001816), Instruction(872882186), Instruction(1275553331), Instruction(201695287), Instruction(335921175), Instruction(1610619778), Instruction(1141335452), Instruction(168648799), Instruction(1678073857), Instruction(1141076863), Instruction(805593137), Instruction(1610620893), Instruction(470319165), Instruction(1208249234), Instruction(1476450620), Instruction(805732352), Instruction(805331002), Instruction(1476404477), Instruction(1409807818), Instruction(155246189), Instruction(268959788), Instruction(1342404056), Instruction(603981556), Instruction(1342664782), Instruction(335798318), Instruction(1745272878), Instruction(1208287125), Instruction(201941063), Instruction(403087420), Instruction(268722211), Instruction(1678327836), Instruction(268517459), Instruction(738738218), Instruction(1141440043), Instruction(1678164037), Instruction(1342841775), Instruction(1141573899), Instruction(201957443), Instruction(200663043), Instruction(797432), Instruction(1275876791), Instruction(202113101), Instruction(136220873), Instruction(269090916), Instruction(1073765745), Instruction(806010916), Instruction(1007285194), Instruction(1543749714), Instruction(1141161982), Instruction(470532112), Instruction(403382378), Instruction(1342538039), Instruction(402661458), Instruction(1006657384), Instruction(1074665000), Instruction(269361260), Instruction(403570800), Instruction(403013673), Instruction(1477211004), Instruction(1208856492), Instruction(939987960), Instruction(1006662738), Instruction(1812848741), Instruction(202063899), Instruction(1074278362), Instruction(537441072), Instruction(1745240134), Instruction(403660848), Instruction(137322763), Instruction(873308245), Instruction(336330873), Instruction(269467691), Instruction(93751562), Instruction(201351259), Instruction(1074347169), Instruction(201326706), Instruction(1275081146), Instruction(1812521077), Instruction(1678344222), Instruction(1276089962), Instruction(1276213419), Instruction(403767347), Instruction(1207977108), Instruction(1208467599), Instruction(537697720), Instruction(403669130), Instruction(1207992290), Instruction(1677738126), Instruction(1141727248), Instruction(536881508), Instruction(134250650), Instruction(1477492609), Instruction(671096833), Instruction(873562150), Instruction(1074074868), Instruction(1074929613), Instruction(269475848), Instruction(1141794890), Instruction(202203257), Instruction(672211069), Instruction(1074946938), Instruction(538130786), Instruction(167338315), Instruction(335552674), Instruction(201859167), Instruction(201572468), Instruction(1544773662), Instruction(1140852849), Instruction(738197586), Instruction(1745920149), Instruction(604230715), Instruction(171797664), Instruction(940894458), Instruction(1209114681), Instruction(469762209), Instruction(404045824), Instruction(403972194), Instruction(806682753), Instruction(1678893210), Instruction(269402242), Instruction(470605851), Instruction(1477877636), Instruction(121964244), Instruction(1142314519), Instruction(202514592), Instruction(269197394), Instruction(538375271), Instruction(1678901370), Instruction(1007926766), Instruction(738787514), Instruction(133904256), Instruction(1746043057), Instruction(1543512214), Instruction(754492), Instruction(202457269), Instruction(1813348538), Instruction(1811939495), Instruction(269770927), Instruction(469778537), Instruction(402653271), Instruction(404029601), Instruction(1142145375), Instruction(538216918), Instruction(806731928), Instruction(536998028), Instruction(1142090525), Instruction(135177410), Instruction(117843804), Instruction(202727558), Instruction(469786800), Instruction(337174734), Instruction(739590144), Instruction(201343168), Instruction(404365342), Instruction(269631685), Instruction(1477599455), Instruction(605585507), Instruction(603990020), Instruction(1678622925), Instruction(1679286384), Instruction(70353162), Instruction(202432614), Instruction(143615051), Instruction(873996374), Instruction(1209219726), Instruction(337281240), Instruction(538721388), Instruction(167626416), Instruction(1209588694), Instruction(1276296542), Instruction(873947358), Instruction(168706097), Instruction(471638200), Instruction(874242213), Instruction(1746108606), Instruction(672792770), Instruction(1746297021), Instruction(807002331), Instruction(807026920), Instruction(270360659), Instruction(1746501845), Instruction(130782443), Instruction(1612577768), Instruction(1746763976), Instruction(110063722), Instruction(1679704273), Instruction(1276562779), Instruction(402653206), Instruction(1209835867), Instruction(806445296), Instruction(1679728888), Instruction(1677738234), Instruction(1612466016), Instruction(1343988654), Instruction(1142016995), Instruction(269631608), Instruction(1611675422), Instruction(939544289), Instruction(403308755), Instruction(1207062), Instruction(538534743), Instruction(1210072875), Instruction(167094496), Instruction(1745469700), Instruction(404545777), Instruction(1477447785), Instruction(75679747), Instruction(168375961), Instruction(1813537030), Instruction(1478533443), Instruction(270524667), Instruction(805765334), Instruction(1545445561), Instruction(1477091073), Instruction(1277268873), Instruction(1008748148), Instruction(1545666834), Instruction(1545682946), Instruction(538523169), Instruction(337821969), Instruction(337805581), Instruction(1612448655), Instruction(1411210257), Instruction(539047419), Instruction(1746714776), Instruction(1210118594), Instruction(139910323), Instruction(404676867), Instruction(404922654), Instruction(740516127), Instruction(404906273), Instruction(203194519), Instruction(167410508), Instruction(472154403), Instruction(874627299), Instruction(77136433), Instruction(807100540), Instruction(270713120), Instruction(807592233), Instruction(1477029957), Instruction(1478844975), Instruction(1410591293), Instruction(1277085059), Instruction(539342789), Instruction(404217975), Instruction(672743696), Instruction(1813299481), Instruction(874774834), Instruction(1210291004), Instruction(940511853), Instruction(145745106), Instruction(1076289003), Instruction(1277554865), Instruction(1009065728), Instruction(942082750), Instruction(270926030), Instruction(202260795), Instruction(1613078228), Instruction(1009232587), Instruction(1478897723), Instruction(405217594), Instruction(203858238), Instruction(941949551), Instruction(152453601), Instruction(672874638), Instruction(1411914656), Instruction(472056131), Instruction(338215233), Instruction(158151380), Instruction(469778727), Instruction(1546133826), Instruction(740573184), Instruction(874725596), Instruction(740188160), Instruction(874365251), Instruction(740933633), Instruction(1141040337), Instruction(269762891), Instruction(1277801678), Instruction(605367239), Instruction(942291898), Instruction(606732997), Instruction(1412086018), Instruction(402669571), Instruction(1747599589), Instruction(1479203173), Instruction(1143561274), Instruction(1075896990), Instruction(673906690), Instruction(1344938704), Instruction(1076557141), Instruction(405315833), Instruction(405332292), Instruction(1076629684), Instruction(1209962953), Instruction(1210836677), Instruction(1545576621), Instruction(539762756), Instruction(1814716771), Instruction(1412125605), Instruction(1610624294), Instruction(154996969), Instruction(1411982716), Instruction(1546117428), Instruction(807706812), Instruction(472367466), Instruction(1345099216), Instruction(539869172), Instruction(1546051933), Instruction(1814307170), Instruction(942532175), Instruction(942315216), Instruction(673980774), Instruction(1613128651), Instruction(1343942928), Instruction(338272626), Instruction(607058403), Instruction(1680720248), Instruction(203866462), Instruction(1076845078), Instruction(1680687420), Instruction(1009459339), Instruction(942246720), Instruction(1814135038), Instruction(1479520525), Instruction(673186128), Instruction(338526571), Instruction(1210647877), Instruction(159928354), Instruction(1613791390), Instruction(1345259701), Instruction(1478841119), Instruction(1278018044), Instruction(539916535), Instruction(1278256190), Instruction(1677721953), Instruction(1546199431), Instruction(674308488), Instruction(1345423001), Instruction(1479320244), Instruction(145516587), Instruction(1411512523), Instruction(674316686), Instruction(1009887282), Instruction(807370753), Instruction(808255890), Instruction(1008548425), Instruction(1211091435), Instruction(336625795), Instruction(606837498), Instruction(144601543), Instruction(1479665890), Instruction(808230914), Instruction(1010006009), Instruction(1077125902), Instruction(204702101), Instruction(1677721987), Instruction(270622824), Instruction(1412703330), Instruction(1813717363), Instruction(1412664619), Instruction(1342182183), Instruction(271540568), Instruction(134389831), Instruction(1278531598), Instruction(1479871128), Instruction(741474722), Instruction(1479891288), Instruction(1278567891), Instruction(271917484), Instruction(338714626), Instruction(1412573775), Instruction(740532640), Instruction(271180162), Instruction(741589377), Instruction(472572325), Instruction(539984817), Instruction(472736173), Instruction(540436860), Instruction(1479832995), Instruction(473170327), Instruction(1546969390), Instruction(673931702), Instruction(405979576), Instruction(876036539), Instruction(204374337), Instruction(1613856610), Instruction(272032189), Instruction(741138876), Instruction(271581632), Instruction(1010287255), Instruction(177406507), Instruction(271524279), Instruction(1010325397), Instruction(540552162), Instruction(1345888142), Instruction(1077282373), Instruction(204874101), Instruction(1681432940), Instruction(1614017882), Instruction(540638215), Instruction(406012364), Instruction(741597645), Instruction(156244171), Instruction(540010644), Instruction(1144653066), Instruction(1278861779), Instruction(1614434972), Instruction(205135874), Instruction(741917139), Instruction(674832820), Instruction(406487489), Instruction(204227021), Instruction(741597552), Instruction(145097795), Instruction(742072794), Instruction(205177236), Instruction(607873270), Instruction(741720438), Instruction(205169118), Instruction(1681580504), Instruction(1010370864), Instruction(473702832), Instruction(339157429), Instruction(674996700), Instruction(1346131572), Instruction(1480355134), Instruction(1547469284), Instruction(1681474022), Instruction(943501105), Instruction(809034179), Instruction(67108923), Instruction(406651369), Instruction(1278922931), Instruction(271835616), Instruction(146672474), Instruction(194361276), Instruction(540802072), Instruction(205111792), Instruction(1412589580), Instruction(205357551), Instruction(540918517), Instruction(339624433), Instruction(674873845), Instruction(674046399), Instruction(1346312136), Instruction(674439611), Instruction(742302183), Instruction(540986482), Instruction(540910490), Instruction(608134395), Instruction(205505022), Instruction(943713087), Instruction(339730923), Instruction(1010784313), Instruction(473883136), Instruction(163315196), Instruction(1614651455), Instruction(541090599), Instruction(1144940014), Instruction(1480539718), Instruction(473932286), Instruction(1279191422), Instruction(1010893504), Instruction(204521906), Instruction(339796485), Instruction(272597515), Instruction(406544896), Instruction(339747236), Instruction(675398143), Instruction(272753162), Instruction(541200084), Instruction(1346447097), Instruction(205570566), Instruction(67108912), Instruction(1614962713), Instruction(67108936), Instruction(205693458), Instruction(675488278), Instruction(272843287), Instruction(272728593), Instruction(1547895309), Instruction(608389003), Instruction(474178059), Instruction(407077380), Instruction(407085598), Instruction(205685265), Instruction(1011102736), Instruction(1682178593), Instruction(205816353), Instruction(272908837), Instruction(205808166), Instruction(540402466), Instruction(742171167), Instruction(608495987), Instruction(1010849756), Instruction(1011170253), Instruction(474300824), Instruction(944074985), Instruction(272998900), Instruction(742752808), Instruction(205906410), Instruction(205898176), Instruction(742793646), Instruction(205930865), Instruction(205914406)),3,3,563)
    println(trial(start).map( _.floor.toChar ).take(string.length*2).mkString)

    run( 1, start, EvolverStrategy(12, 0.0001625, optimiseForPipeline = false))

    System.exit(0)
  }


}
