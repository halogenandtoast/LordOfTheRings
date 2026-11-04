module LordOfTheRings.Treachery.Cards.SneakingOff (sneakingOff) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype SneakingOff = SneakingOff TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sneakingOff :: TreacheryCard SneakingOff
sneakingOff = treachery SneakingOff Cards.sneakingOff

{- | When Revealed: The first player loses control of Urdug, then attach Urdug to a
location in the staging area as a guarded objective. If he cannot, add 5 to the total
threat in the staging area until the end of the phase. In either case, remove Sneaking Off
from the game.

The first player chooses the location. A first player who does not control Urdug, as
while Urdug is guarded or an enemy, cannot.
-}
instance RunMessage SneakingOff where
  runMessage msg t@(SneakingOff attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      urdug <- select (AllyWithCardCode (toCardCode Objectives.urdug) <> AllyControlledBy (PlayerWithId lead))
      locations <- select LocationInStagingArea
      case urdug of
        u : _ | notNull locations -> chooseOrRunOneM lead do
          questionLabeled "sneakingOff.location"
          causedBy attrs
          targets locations \l -> guardCharacterWith u =<< getLocation l
        _ -> phaseModifier attrs GameTarget (StagingThreatBonus 5)
      removeFromGame attrs
      pure t
    _ -> SneakingOff <$> runMessage msg attrs
