-- | What The Battle of the Pelennor Fields' cards share: Minas Tirith and the damage dealt
-- to it, the Harad enemies, and the enemies the encounter deck is searched for.
module LordOfTheRings.Scenario.Scenarios.TheBattleOfThePelennorFields.Helpers (
  getMinasTirith,
  minasTirithHitPoints,
  damageMinasTirith,
  chooseDamageOrMinasTirith,
  discardUntilEnemyToStaging,
  attachedCopies,
  harad,
) where

import LordOfTheRings.Card (CardType (EnemyType), toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.I18n (intVar)
import LordOfTheRings.Entities (entitiesObjectives, entitiesTreacheries)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Id
import LordOfTheRings.Matcher
import LordOfTheRings.Message (Message)
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Source (Sourceable)
import LordOfTheRings.Target
import LordOfTheRings.Trait (Trait (Harad))
import LordOfTheRings.Card.CardDef (CardDef)
import LordOfTheRings.Treachery.Types (TreacheryAttrs (..))

-- | Minas Tirith, while it is in play: from stage 3A on.
getMinasTirith :: HasGame m => m (Maybe ObjectiveId)
getMinasTirith =
  getsGame
    $ headMay
    . map toId
    . filter ((== toCardCode Objectives.minasTirith) . toCardCode)
    . toList
    . entitiesObjectives
    . gameEntities

-- | Minas Tirith has 30 hit points plus 10 for each player.
minasTirithHitPoints :: HasGame m => m Int
minasTirithHitPoints = (\n -> 30 + 10 * n) . length <$> getRemainingPlayers

{- | Damage dealt to Minas Tirith, nothing while it is not in play: an effect that targets it
at stage 2B resolves as fully as it can (MEC54 rules, "Minas Tirith"). An active City Wall
takes it first (see 'LordOfTheRings.Modifier.DamagePlacedFirstOn').
-}
damageMinasTirith :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
damageMinasTirith source n = getMinasTirith >>= traverse_ \o -> dealDamage source (ObjectiveTarget o) n

{- | "Either <the other effect>, or deal X damage to Minas Tirith": the player chooses, the
other effect taken alone while Minas Tirith is not in play, which must then be resolved as
fully as possible (MEC54 rules, "Minas Tirith").
-}
chooseDamageOrMinasTirith
  :: (ReverseQueue m, Targetable source, Sourceable source)
  => source
  -> PlayerId
  -> Text
  -> Int
  -> QueueT Message m ()
  -> m ()
chooseDamageOrMinasTirith source pid key n other = do
  mt <- getMinasTirith
  chooseOrRunOneM pid do
    questionLabeled (key <> ".choose")
    causedBy source
    labeled (key <> ".other") other
    for_ mt \_ -> labeledWith "minasTirith.damage" [intVar "n" n] $ damageMinasTirith source n

{- | Cards are discarded from the top of the encounter deck until an enemy is, and that
enemy is added to the staging area, as The Witch-king and Siege Tower read.
-}
discardUntilEnemyToStaging :: (ReverseQueue m, Sourceable a, Targetable a) => a -> m ()
discardUntilEnemyToStaging a = discardEncounterCardsUntilThen a (CardWithType EnemyType) addToStagingFromDiscard

-- | The copies of the treachery attached to the card, as "Limit 1 per hero" counts them.
attachedCopies :: (HasGame m, Targetable host) => CardDef -> host -> m Int
attachedCopies def host =
  getsGame
    $ length
    . filter (\t -> toCardCode t == toCardCode def && treacheryPlacement (toAttrs t) == AttachedTo (toTarget host))
    . toList
    . entitiesTreacheries
    . gameEntities

-- | A Harad enemy card, as the campaign log records them.
harad :: CardMatcher
harad = CardMatchAll [CardWithType EnemyType, CardWithTrait Harad]
