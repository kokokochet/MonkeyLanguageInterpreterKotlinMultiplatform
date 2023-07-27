import kotlin.jvm.JvmInline

sealed interface Object {
    fun inspect(): String
}

@JvmInline
value class Integer(val value: Long): Object {

    override fun inspect(): String {
        return value.toString()
    }
}

@JvmInline
value class BooleanObj(val value: Boolean): Object {
    override fun inspect(): String {
        return value.toString()
    }
}


object Null: Object {

    override fun inspect(): String {
        return "null"
    }
}