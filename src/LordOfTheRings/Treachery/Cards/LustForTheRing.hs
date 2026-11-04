module LordOfTheRings.Treachery.Cards.LustForTheRing (lustForTheRing) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Saga (oneRings)
import LordOfTheRings.Sphere (Sphere (Fellowship))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LustForTheRing = LustForTheRing TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of The Ring Goes South's burden set.
lustForTheRing :: TreacheryCard LustForTheRing
lustForTheRing = treachery LustForTheRing Cards.lustForTheRing

-- | Counts as a Condition attachment with the text: "Forced: After The One Ring exhausts,
-- raise each player's threat by 1 and reduce attached hero's willpower to 0 until the end
-- of the round." Any box's The One Ring, as the burden follows the campaign.
instance HasAbilities LustForTheRing where
  getAbilities (LustForTheRing a) = case a.placement of
    AttachedTo (HeroTarget _) -> [mkAbility a i $ forced $ ObjectiveExhaustedWindow After ring | (i, ring) <- zip [1 ..] oneRings]
    _ -> []

-- | Peril. Surge. When Revealed: Attach to a non-Fellowship hero in play, the player who
-- revealed it choosing.
instance RunMessage LustForTheRing where
  runMessage msg t@(LustForTheRing attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      heroes <- select (HeroControlledBy AnyPlayer <> not_ (HeroWithSphere Fellowship))
      chooseOrRunOneM you do
        questionLabeled "lustForTheRing.attach"
        causedBy attrs
        targets heroes \h -> attachTreachery attrs.treacheryId h
      pure t
    UseCardAbility _ (isSource attrs -> True) _ _ -> do
      eachPlayer \pid -> encounterRaisesThreat pid 1
      case attrs.placement of
        AttachedTo (HeroTarget h) -> roundModifier attrs h WillpowerReducedToZero
        _ -> pure ()
      pure t
    _ -> LustForTheRing <$> runMessage msg attrs
