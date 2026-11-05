module LordOfTheRings.Game.Modifiers (module LordOfTheRings.Game.Modifiers) where

import Control.Monad.Writer.Strict (execWriterT)
import Data.Map.Monoidal.Strict (MonoidalMap (..))
import Data.Map.Strict qualified as Map (
  findWithDefault,
  unionWith,
 )
import Data.UUID qualified as UUID (nil)
import LordOfTheRings.ActiveCost (
  ActiveCost (activeCostCard, activeCostPlayer),
 )
import LordOfTheRings.Ally (createAlly)
import LordOfTheRings.Attachment (lookupAttachment)
import LordOfTheRings.Card (
  Card (..),
  CardDef (cdCardType),
  CardType (AllyType, EventType, PlayerSideQuestType),
  isAttachmentType,
  HasCardCode (toCardCode),
  HasCardDef (toCardDef),
 )
import LordOfTheRings.Classes.Entity (
  Entity (overAttrs, toAttrs, toId),
 )
import LordOfTheRings.Classes.HasModifiersFor (
  HasModifiersFor (getModifiersFor),
  HasModifiersM,
 )
import LordOfTheRings.Entities (heroesL)
import LordOfTheRings.Event (lookupEvent)
import LordOfTheRings.Game.Base (
  Game (gameActiveCosts, gameModifiers),
  entitiesL,
 )
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Hero (asGainedText, withGainedAbilities)
import LordOfTheRings.Id (
  AllyId (AllyId),
  AttachmentId (AttachmentId),
  PlayerId,
  QuestId (QuestId),
 )
import LordOfTheRings.Modifier (
  Modifier (Modifier),
  ModifierType (GainGameTextOf),
 )
import LordOfTheRings.Prelude hiding (first, many)
import LordOfTheRings.Quest (QuestAttrs (questOwner), lookupQuest)
import LordOfTheRings.Target (Target (HeroTarget))

gainedAbilities :: Game -> Game
gainedAbilities g = g & entitiesL . heroesL %~ fmap \h ->
  withGainedAbilities [code | Modifier _ (GainGameTextOf code) <- Map.findWithDefault [] (HeroTarget $ toId h) (gameModifiers g)] h

{- | The modifiers cards in hand give themselves, as what they cost to play: each ally and
event in a hand, or being paid for, is built from its card to be asked. A player side quest
in the victory display is built from its card too, for what it gives while there, as The
Storm Comes reads "while this quest is in the victory display" (see 'inVictoryDisplay').
-}
withHandModifiers :: Game -> GameT Game
withHandModifiers g = do
  MonoidalMap fromHands <- execWriterT do
    for_ g.players \player -> for_ player.hand (handCardModifiers player.id)
    for_ (gameActiveCosts g) \ac -> handCardModifiers ac.activeCostPlayer ac.activeCostCard
    for_ (toAttrs g.scenario).victoryDisplay \case
      PlayerCard pc | cdCardType (toCardDef pc) == PlayerSideQuestType, Just owner <- pc.owner ->
        getModifiersFor $ overAttrs (\a -> a {questOwner = Just owner}) $ lookupQuest (toCardCode pc) pc.id (QuestId UUID.nil)
      _ -> pure ()
    -- the constant abilities of a hero card whose game text a hero gains (Thorongil)
    for_ g.entities.heroes \h ->
      for_ [code | Modifier _ (GainGameTextOf code) <- Map.findWithDefault [] (HeroTarget $ toId h) (gameModifiers g)] \code ->
        getModifiersFor (asGainedText code h)
  pure
    $ if null fromHands then g else g {gameModifiers = Map.unionWith (<>) (gameModifiers g) fromHands}

-- | The modifiers a card gives itself from its owner's hand, or while being paid for.
handCardModifiers :: HasModifiersM m => PlayerId -> Card -> m ()
handCardModifiers pid = \case
  PlayerCard pc -> case cdCardType (toCardDef pc) of
    AllyType -> getModifiersFor $ createAlly pc (AllyId UUID.nil) pid
    EventType -> traverse_ getModifiersFor $ lookupEvent (toCardCode pc) pc.id pid
    -- an attachment's builder is unplaced, as a Record that costs less in hand reads it
    t | isAttachmentType t -> getModifiersFor $ lookupAttachment (toCardCode pc) pc.id (AttachmentId UUID.nil) pid
    _ -> pure ()
  EncounterCard _ -> pure ()

