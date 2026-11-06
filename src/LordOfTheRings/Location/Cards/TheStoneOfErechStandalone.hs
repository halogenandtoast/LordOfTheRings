module LordOfTheRings.Location.Cards.TheStoneOfErechStandalone (theStoneOfErechStandalone) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Window (exhaustableCharacters)
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Classes.HasGame (HasGame)

newtype TheStoneOfErechStandalone = TheStoneOfErechStandalone LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theStoneOfErechStandalone :: LocationCard TheStoneOfErechStandalone
theStoneOfErechStandalone = location TheStoneOfErechStandalone Cards.theStoneOfErechStandalone

instance HasModifiersFor TheStoneOfErechStandalone where
  getModifiersFor (TheStoneOfErechStandalone a) = do
    players <- getPlayerOrder
    low <- lowWillpower
    eligible <- exhaustableCharacters AnyHero AnyAlly
    modifySelf a $ [ImmuneToPlayerCardEffects, ThreatModifier $ 2 * length players] <> [CannotBeTraveledTo | any (`notElem` eligible) low]
    when (a.placement == ActiveLocation) do
      characters <- selectCharacters AnyHero AnyAlly
      for_ characters \target -> modified a target [CannotReadyWithWillpowerBelow 2]

instance RunMessage TheStoneOfErechStandalone where
  runMessage msg l@(TheStoneOfErechStandalone a) = case msg of
    TravelTo lid | lid == a.id -> do
      traverse_ exhaust =<< lowWillpower
      pure l
    _ -> TheStoneOfErechStandalone <$> runMessage msg a

lowWillpower :: HasGame m => m [Target]
lowWillpower = filterM (fmap (< 2) . getWillpower) =<< selectCharacters AnyHero AnyAlly
