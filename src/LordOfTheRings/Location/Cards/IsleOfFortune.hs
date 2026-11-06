module LordOfTheRings.Location.Cards.IsleOfFortune (isleOfFortune) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype IsleOfFortune = IsleOfFortune LocationAttrs
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, Entity)

instance IsLocation IsleOfFortune where
  initialResourcesPerPlayer _ = 1

isleOfFortune :: LocationCard IsleOfFortune
isleOfFortune = location IsleOfFortune Cards.isleOfFortune

instance HasAbilities IsleOfFortune where
  getAbilities (IsleOfFortune a) =
    [mkAbility a 1 $ forced $ MatchingEnemyEntersPlayWindow After shipEnemy]
      <> [ mkAbility a 2 $ response $ ExploredWindow When a.id
         | a.placement == ActiveLocation
         , countTokens Resource a.locationTokens > 0
         ]

instance RunMessage IsleOfFortune where
  runMessage msg l@(IsleOfFortune attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      removeTokens attrs attrs Resource 1
      pure l
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      doStep (countTokens Resource attrs.locationTokens) msg
      pure l
    DoStep n inner@(UseCardAbility pid (isSource attrs -> True) 2 _) | n > 0 -> do
      heroes <- select AnyHero
      chooseOneM pid do
        questionLabeled "dreadnaught.isleOfFortune"
        causedBy attrs
        targets heroes \hero -> do
          removeTokens attrs attrs Resource 1
          placeResources attrs hero 1
          doStep (n - 1) inner
      pure l
    _ -> IsleOfFortune <$> runMessage msg attrs
