module LordOfTheRings.Treachery.Cards.BelegostTreacheries
  ( echoesInTheDark, spawnOfThangorodrim, concealedSpikes, coveredPit ) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (CardType (EventType), cdCardType, cdKeywords, toCardDef, toCardCode)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Window (shadowCardCancelable)
import LordOfTheRings.Keyword (Keyword (Permanent))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheRuinsOfBelegost.Helpers
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BelegostTreacheries = BelegostTreacheries TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

echoesInTheDark, spawnOfThangorodrim, concealedSpikes, coveredPit :: TreacheryCard BelegostTreacheries
echoesInTheDark = treachery BelegostTreacheries Cards.echoesInTheDark
spawnOfThangorodrim = treachery BelegostTreacheries Cards.spawnOfThangorodrim
concealedSpikes = treachery BelegostTreacheries Cards.concealedSpikes
coveredPit = treachery BelegostTreacheries Cards.coveredPit

data BelegostTreacheryKind = Echoes | Spawn | Spikes | Pit

treacheryKind :: TreacheryAttrs -> BelegostTreacheryKind
treacheryKind a
  | toCardCode a == toCardCode Cards.echoesInTheDark = Echoes
  | toCardCode a == toCardCode Cards.spawnOfThangorodrim = Spawn
  | toCardCode a == toCardCode Cards.concealedSpikes = Spikes
  | otherwise = Pit

instance RunMessage BelegostTreacheries where
  runMessage msg t@(BelegostTreacheries a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      case treacheryKind a of
        Echoes -> phaseModifier a GameTarget StagingThreatPerFirstPlayerCharacter
        Spawn -> do
          dragons <- select $ EnemyWithCardCode (toCardCode Enemies.naurlhug)
          if null dragons then do
            placeStalkingResources a 1
            doStep 1 msg
          else do
            lead <- getLead
            for_ dragons $ immediateAttack lead
        Spikes -> do
          active <- select IsActiveLocation
          x <- (1 +) . sum . map (fromMaybe 0) <$> traverse getDiscoverValue active
          eachPlayer \pid -> dealDamageDivided a pid x
        Pit -> eachPlayer \pid -> do
          allies <- select $ AllyControlledBy (PlayerWithId pid)
          chooseOrRunOneM pid do
            questionLabeled "belegost.discardAlly"
            causedBy a
            targets allies (discard a)
      pure t
    DoStep 1 (WhenRevealed (isTarget a -> True)) -> do
      whenM ((>= 3) <$> stalkingResources) summonNaurlhug
      pure t
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      case treacheryKind a of
        Echoes -> discardRandomFromHandThen pid 1 (DoStep 1 msg)
        Spawn -> additionalAttack pid eid
        Spikes -> do
          attachments <- getAttachmentsControlledBy pid
          highest <- filterM canDiscardForSpikes =<< highestCostAttachmentsControlledBy pid
          gameMods <- getModifiers GameTarget
          shadowCancelable <- shadowCardCancelable cid
          let x = length attachments
              cancelable = shadowCancelable && CannotCancelEncounterEffects `notElem` gameMods
          when (x > 0) $ chooseOrRunOneM pid do
            questionLabeled "belegost.spikesShadow"
            causedBy a
            labeled "belegost.spikesDamage" $ dealDamageDivided a pid x
            when (cancelable && notNull highest) $ labeled "belegost.spikesDiscard" $ chooseOrRunOneM pid do
              questionLabeled "discardHighestCostAttachment"
              causedBy a
              targets highest \target -> do
                discard a target
                push $ WithTarget target (DoStep 2 msg)
        Pit -> do
          unique <- selectAny $ EnemyWithId eid <> UniqueEnemy
          unless unique do
            loot <- lootControlledBy pid
            enemy <- getEnemy eid
            chooseOrRunOneM pid do
              questionLabeled "belegost.guardedLoot"
              causedBy a
              targets loot \o -> do
                placeObjective o.id StagingArea
                guardObjectiveWith o.id (toAttrs enemy).cardId
          -- This sentence applies even if the attacker is unique or no Loot moved.
          ifThisAttackDestroys eid msg
      pure t
    WithDiscardedRandomCards _ cards (DoStep 1 (ResolveShadowEffect cid eid _ _)) | cid == a.cardId -> do
      when (any ((== EventType) . cdCardType . toCardDef) cards) $ enemyAttackModifier a eid (AttackModifier 2)
      pure t
    WithTarget target (DoStep 2 (ResolveShadowEffect cid _ pid _)) | cid == a.cardId -> do
      present <- attachmentPresent target
      if present then dealDamageDivided a pid . length =<< getAttachmentsControlledBy pid
      else push $ CheckWindows [mkAfter $ EncounterEffectCanceled $ CardIdTarget cid]
      pure t
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == a.cardId -> do
      placeEnemy eid StagingArea
      pure t
    _ -> BelegostTreacheries <$> runMessage msg a

canDiscardForSpikes :: HasGame m => Target -> m Bool
canDiscardForSpikes target = do
  mods <- getModifiers target
  permanent <- getsGame \g -> case target of
    AttachmentTarget aid -> maybe False ((Permanent `member`) . cdKeywords . toCardDef) (Map.lookup aid g.entities.attachments)
    _ -> False
  pure $ not permanent && CannotLeavePlay `notElem` mods

attachmentPresent :: HasGame m => Target -> m Bool
attachmentPresent target = getsGame \g -> case target of
  AttachmentTarget aid -> Map.member aid g.entities.attachments
  ObjectiveTarget oid -> Map.member oid g.entities.objectives
  _ -> False
