module LordOfTheRings.Treachery.Cards.AFoeBeyond (aFoeBeyond) where

import LordOfTheRings.Placement (Placement (Unplaced))
import LordOfTheRings.Scenario.Scenarios.FlightFromMoria.Helpers (namelessFearAttack)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype AFoeBeyond = AFoeBeyond TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

aFoeBeyond :: TreacheryCard AFoeBeyond
aFoeBeyond = treachery AFoeBeyond Cards.aFoeBeyond

-- | While it is being revealed, its "when revealed" effect cannot be canceled.
instance HasModifiersFor AFoeBeyond where
  getModifiersFor (AFoeBeyond a) = when (a.placement == Unplaced) $ modified a GameTarget [CannotCancelEncounterEffects]

-- | When Revealed: The last player deals damage equal to The Nameless Fear's attack to a
-- hero he controls. This effect cannot be canceled.
-- Shadow: Deal damage equal to The Nameless Fear's attack to the defending character.
instance RunMessage AFoeBeyond where
  runMessage msg t@(AFoeBeyond attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lastPlayer <- getLastPlayer
      n <- namelessFearAttack
      heroes <- select (HeroControlledBy $ PlayerWithId lastPlayer)
      chooseOrRunOneM lastPlayer do
        questionLabeled "aFoeBeyond.hero"
        causedBy attrs
        targets heroes \h -> dealDamage attrs h n
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      n <- namelessFearAttack
      for_ defenders \d -> dealDamage attrs d n
      pure t
    _ -> AFoeBeyond <$> runMessage msg attrs
