module LordOfTheRings.Ally.Cards.Gloin (gloin) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Trait (Trait (Dwarf))

newtype Gloin = Gloin AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gloin :: AllyCard Gloin
gloin = ally Gloin Cards.gloin

-- | While you control at least 5 Dwarf characters, Glóin gains: "Response: After you
-- play Glóin from your hand, choose a hero. Add 2 resources to that hero's resource
-- pool."
instance HasAbilities Gloin where
  getAbilities (Gloin a) =
    [ restricted a 1 (CharactersAtLeast 5 (HeroControlledBy you <> HeroWithTrait Dwarf) (AllyControlledBy you <> AllyWithTrait Dwarf))
        $ response
        $ PlayedFromHandWindow After (CardIdTarget a.allyCardId)
    ]
   where
    you = PlayerWithId a.owner

instance RunMessage Gloin where
  runMessage msg s@(Gloin a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      heroes <- select AnyHero
      chooseOneM pid do
        questionLabeled "gloin.chooseHero"
        causedBy a
        targets heroes \h -> placeResources a h 2
      pure s
    _ -> Gloin <$> runMessage msg a
