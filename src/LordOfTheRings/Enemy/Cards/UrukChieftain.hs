module LordOfTheRings.Enemy.Cards.UrukChieftain (urukChieftain) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Trait (Trait (Orc))

newtype UrukChieftain = UrukChieftain EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

urukChieftain :: EnemyCard UrukChieftain
urukChieftain = enemy UrukChieftain Cards.urukChieftain

-- | Cannot have player card attachments or be optionally engaged ('CannotHaveAttachments'
-- bars player attachments only: a Mordor Warg still attaches to it). While the engaged
-- player is engaged with another Orc enemy, Uruk Chieftain cannot take damage.
instance HasModifiersFor UrukChieftain where
  getModifiersFor (UrukChieftain a) = do
    modifySelf a [CannotHaveAttachments, CannotBeOptionallyEngagedBy AnyPlayer]
    case a.placement of
      EngagedWith pid -> do
        guarded <- selectAny (EnemyEngagedWith (PlayerWithId pid) <> EnemyWithTrait Orc <> not_ (EnemyWithId a.id))
        when guarded $ modifySelf a [CannotTakeDamage]
      _ -> pure ()

-- | Forced: At the end of the encounter phase, Uruk Chieftain engages the player engaged
-- with the most non-unique Orc enemies, the first player choosing among those tied.
instance HasAbilities UrukChieftain where
  getAbilities (UrukChieftain a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When EncounterPhase]

instance RunMessage UrukChieftain where
  runMessage msg e@(UrukChieftain attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      players <- getRemainingPlayers
      counts <- for players \p -> (p,) . length <$> select (EnemyEngagedWith (PlayerWithId p) <> EnemyWithTrait Orc <> not_ UniqueEnemy)
      let most = foldr (max . snd) 0 counts
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "urukChieftain.engage"
        causedBy attrs
        targets [p | (p, n) <- counts, n == most] \p ->
          unless (attrs.placement == EngagedWith p) $ engageEnemy p attrs.id
      pure e
    _ -> UrukChieftain <$> runMessage msg attrs
