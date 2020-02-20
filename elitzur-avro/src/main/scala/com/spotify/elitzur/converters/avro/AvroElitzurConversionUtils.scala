package com.spotify.elitzur.converters.avro

import java.nio.ByteBuffer

import org.apache.avro.Schema
import org.apache.avro.generic.{GenericData, GenericRecord, GenericRecordBuilder}

import scala.collection.JavaConverters._

object AvroElitzurConversionUtils {
  private[elitzur] def getAvroField(r: GenericRecord, fieldName: Seq[String]): Object = {
    fieldName match {
      case name :: Nil => r.get(name)
      case name :: "innerData" :: Nil => r.get(name)
      case head :: tail => getAvroField(r.get(head).asInstanceOf[GenericRecord], tail)
    }
  }

  private[elitzur] def convertOptional[T](v: java.util.Optional[T]): Option[T] = {
    if (v.isPresent) Option(v.get()) else None
  }

  def byteBufferToByteArray(bBuffer: ByteBuffer): Array[Byte] = {
    // http://errorprone.info/bugpattern/ByteBufferBackingArray
    val bArray = new Array[Byte](bBuffer.remaining)
    bBuffer.get(bArray)
    bBuffer.position(bBuffer.position() - bArray.length) // Restores position
    bArray
  }

  private[elitzur] def recordToGenericData[T <: GenericRecord](record: T): GenericData.Record = {
    val defaultBuilder = new GenericRecordBuilder(record.getSchema)
    record.getSchema.getFields.asScala.foreach { f =>
      defaultBuilder.set(f.name(), record.get(f.name()))
    }
    defaultBuilder.build()
  }

  private[elitzur] def isAvroRecordType(schema: Schema): Boolean =
    Schema.Type.RECORD.equals(schema.getType) ||
      (Schema.Type.UNION.equals(schema.getType) &&
        schema.getTypes.asScala.map(_.getType).contains(Schema.Type.RECORD))


  private[elitzur] def isAvroArrayType(schema: Schema): Boolean =
    Schema.Type.ARRAY.equals(schema.getType) ||
      (Schema.Type.UNION.equals(schema.getType) && schema.getTypes.contains(Schema.Type.ARRAY))
}