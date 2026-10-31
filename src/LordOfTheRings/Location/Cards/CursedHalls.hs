module LordOfTheRings.Location.Cards.CursedHalls (cursedHalls) where

import LordOfTheRings.Card (toCardCode, toCardDef)
import LordOfTheRings.Card.Synthetic qualified as Synthetic
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Entities (entitiesTreacheries)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))
import LordOfTheRings.Trait (Trait (Sorcery))

newtype CursedHalls = CursedHalls LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

cursedHalls :: LocationCard CursedHalls
cursedHalls = location CursedHalls Cards.cursedHalls

-- | While Cursed Halls is in the staging area, Sorcery attachments are immune to player
-- card effects.
instance HasModifiersFor CursedHalls where
  getModifiersFor (CursedHalls a) = when (a.placement == StagingArea) do
    treacheries <- getsGame (toList . entitiesTreacheries . gameEntities)
    for_ [t | t <- treacheries, AttachedTo _ <- [t.placement], Sorcery `member` (toCardDef t).traits] \t ->
      modified a t [ImmuneToPlayerCardEffects]

-- | Shadow: Until the end of the phase, attacking enemy gets +1 attack and +1 defense for
-- each Reanimated Dead engaged with you.
instance RunMessage CursedHalls where
  runMessage msg l@(CursedHalls a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      n <- length <$> select (EnemyWithCardCode (toCardCode Synthetic.reanimatedDead) <> EnemyEngagedWith (PlayerWithId pid))
      when (n > 0) $ phaseModifiers (CardIdSource cid) eid [AttackModifier n, DefenseModifier n]
      pure l
    _ -> CursedHalls <$> runMessage msg a
