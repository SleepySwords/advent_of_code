package com.github.sleepyswords

import java.util.*

sealed class Packet(val version: Long, val type_id: Long)

open class Operator(version: Long, type_id: Long, val packets: List<Packet>) : Packet(version, type_id) {
    override fun toString(): String {
        return "com.github.sleepyswords.Operator{version=$version, type_id=$type_id, packets=$packets}"
    }
}

class Add(version: Long, type_id: Long, packets: List<Packet>): Operator(version, type_id, packets) {
    override fun toString(): String {
        return "Add{version=$version, type_id=$type_id, packets=$packets}"
    }
}
class Product(version: Long, type_id: Long, packets: List<Packet>): Operator(version, type_id, packets) {
    override fun toString(): String {
        return "com.github.sleepyswords.Product{version=$version, type_id=$type_id, packets=$packets}"
    }
}

class Minimum(version: Long, type_id: Long, packets: List<Packet>): Operator(version, type_id, packets) {
    override fun toString(): String {
        return "com.github.sleepyswords.Minimum{version=$version, type_id=$type_id, packets=$packets}"
    }
}

class Maximum(version: Long, type_id: Long, packets: List<Packet>): Operator(version, type_id, packets) {
    override fun toString(): String {
        return "com.github.sleepyswords.Maximum{version=$version, type_id=$type_id, packets=$packets}"
    }
}

class Greater(version: Long, type_id: Long, packets: List<Packet>): Operator(version, type_id, packets) {
    override fun toString(): String {
        return "com.github.sleepyswords.Greater{version=$version, type_id=$type_id, packets=$packets}"
    }
}

class Less(version: Long, type_id: Long, packets: List<Packet>): Operator(version, type_id, packets) {
    override fun toString(): String {
        return "com.github.sleepyswords.Less{version=$version, type_id=$type_id, packets=$packets}"
    }
}

class Equal(version: Long, type_id: Long, packets: List<Packet>): Operator(version, type_id, packets) {
    override fun toString(): String {
        return "com.github.sleepyswords.Equal{version=$version, type_id=$type_id, packets=$packets}"
    }
}

class Literal(version: Long, type_id: Long, val value: Long) : Packet(version, type_id) {
    override fun toString(): String {
        return "com.github.sleepyswords.Literal{version=$version, type_id=$type_id, value=$value}"
    }
}

fun read_bits(data: Queue<Long>, bit_length: Long): String? {
    var value = ""
    for (x in (0 until bit_length)) {
        if (data.peek() == null) { return null }
        value += data.poll().toString()
    }
    return value
}

fun read_long(data: Queue<Long>, bit_length: Long): Long? =
    read_bits(data, bit_length)?.toLong(2)

fun read_literal_value(data: Queue<Long>): Long {
    var value = ""
    while (data.poll() != 0L) {
        value += read_bits(data, 4)
    }
    value += read_bits(data, 4)

    return value.toLong(2)
}

fun read_packet(data: Queue<Long>): Packet? {
    val version = read_long(data, 3)
    val type_id = read_long(data, 3)
    if (version == null || type_id == null) return null
    return when(type_id) {
        4L -> {
            Literal(version, type_id, read_literal_value(data))
        }
        else -> {
            val length_type = read_long(data, 1)
            if (length_type == 1L) {
                val packets = read_long(data, 11)!!
                when(type_id) {
                    0L -> Add(version, type_id, (0 until packets).map { read_packet(data)!! }.toList())
                    1L -> Product(version, type_id, (0 until packets).map { read_packet(data)!! }.toList())
                    2L -> Minimum(version, type_id, (0 until packets).map { read_packet(data)!! }.toList())
                    3L -> Maximum(version, type_id, (0 until packets).map { read_packet(data)!! }.toList())
                    5L -> Greater(version, type_id, (0 until packets).map { read_packet(data)!! }.toList())
                    6L -> Less(version, type_id, (0 until packets).map { read_packet(data)!! }.toList())
                    7L -> Equal(version, type_id, (0 until packets).map { read_packet(data)!! }.toList())
                    else -> Operator(version, type_id, (0 until packets).map { read_packet(data)!! }.toList())
                }
            } else {
                val bits = read_long(data, 15) ?: return null
                val queue = LinkedList((0 until bits).map { data.poll() })
                val packets = mutableListOf<Packet>()
                while (true) {
                    val packet = read_packet(queue) ?: break
                    packets.add(packet)
                }
                when(type_id) {
                    0L -> Add(version, type_id, packets)
                    1L -> Product(version, type_id, packets)
                    2L -> Minimum(version, type_id, packets)
                    3L -> Maximum(version, type_id, packets)
                    5L -> Greater(version, type_id, packets)
                    6L -> Less(version, type_id, packets)
                    7L -> Equal(version, type_id, packets)
                    else -> Operator(version, type_id, packets)
                }
            }
        }
    }
}

fun sum_of_version_numbers(operator: Operator): Long =
    operator.version + operator.packets.sumOf {
        when (it) {
            is Operator -> sum_of_version_numbers(it)
            else -> it.version
        }
    }

fun calculate(operator: Operator): Long {
    val values = operator.packets.map {
        return@map when (it) {
            is Operator -> calculate(it)
            is Literal -> it.value
            else -> { 0L }
        }
    }

    return when(operator) {
        is Add -> values.sum()
        is Product -> values.fold(1L) { acc, l -> acc * l }
        is Minimum -> values.minOrNull()!!
        is Maximum -> values.maxOrNull()!!
        is Greater -> return if (values[0] > values[1]) { 1L } else { 0L }
        is Less -> return if (values[0] < values[1]) { 1L } else { 0L }
        is Equal -> return if (values[0] == values[1]) { 1L } else { 0L }
        else -> 0L
    }
}

val input2 = "E20D41802B2984BD00540010F82D09E35880350D61A41D3004E5611E585F40159ED7AD7C90CF6BD6BE49C802DEB00525272CC1927752698693DA7C70029C0081002140096028C5400F6023C9C00D601ED88070070030005C2201448400E400F40400C400A50801E20004C1000809D14700B67676EE661137ADC64FF2BBAD745B3F2D69026335E92A0053533D78932A9DFE23AC7858C028920A973785338832CFA200F47C81D2BBBC7F9A9E1802FE00ACBA44F4D1E775DDC19C8054D93B7E72DBE7006AA200C41A8510980010D8731720CB80132918319804738AB3A8D3E773C4A4015A498E680292B1852E753E2B29D97F0DE6008CB3D4D031802D2853400D24DEAE0137AB8210051D24EB600844B95C56781B3004F002B99D8F635379EDE273AF26972D4A5610BA51004C12D1E25D802F32313239377B37100105343327E8031802B801AA00021D07231C2F10076184668693AC6600BCD83E8025231D752E5ADE311008A4EA092754596C6789727F069F99A4645008247D2579388DCF53558AE4B76B257200AAB80107947E94789FE76E36402868803F0D62743F00043A1646288800084C3F8971308032996A2BD8023292DF8BE467BB3790047F2572EF004A699E6164C013A007C62848DE91CC6DB459B6B40087E530AB31EE633BD23180393CBF36333038E011CBCE73C6FB098F4956112C98864EA1C2801D2D0F319802D60088002190620E479100622E4358952D84510074C0188CF0923410021F1CE1146E3006E3FC578EE600A4B6C4B002449C97E92449C97E92459796EB4FF874400A9A16100A26CEA6D0E5E5EC8841C9B8FE37109C99818023A00A4FD8BA531586BB8B1DC9AE080293B6972B7FA444285CC00AE492BC910C1697B5BDD8425409700562F471201186C0120004322B42489A200D4138A71AA796D00374978FE07B2314E99BFB6E909678A0"
//val com.github.sleepyswords.getInput2 = "9C005AC2F8F0"
fun main() {
    val map = input2.map {
        return@map when(it) {
            '0' -> arrayOf(0, 0, 0, 0)
            '1' -> arrayOf(0, 0, 0, 1)
            '2' -> arrayOf(0, 0, 1, 0)
            '3' -> arrayOf(0, 0, 1, 1)
            '4' -> arrayOf(0, 1, 0, 0)
            '5' -> arrayOf(0, 1, 0, 1)
            '6' -> arrayOf(0, 1, 1, 0)
            '7' -> arrayOf(0, 1, 1, 1)
            '8' -> arrayOf(1, 0, 0, 0)
            '9' -> arrayOf(1, 0, 0, 1)
            'A' -> arrayOf(1, 0, 1, 0)
            'B' -> arrayOf(1, 0, 1, 1)
            'C' -> arrayOf(1, 1, 0, 0)
            'D' -> arrayOf(1, 1, 0, 1)
            'E' -> arrayOf(1, 1, 1, 0)
            'F' -> arrayOf(1, 1, 1, 1)
            else -> arrayOf()
        }.map { x -> x.toLong() }
    }.flatMap { it.asIterable() }

    val data = LinkedList(map)

    // println("${com.github.sleepyswords.read_packet(data)}")
    println("${calculate(read_packet(data) as Operator)}")
}