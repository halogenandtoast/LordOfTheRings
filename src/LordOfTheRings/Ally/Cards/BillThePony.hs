module LordOfTheRings.Ally.Cards.BillThePony (billThePony) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (Placement (Unplaced))
import LordOfTheRings.Trait (Trait (Hobbit))

newtype BillThePony = BillThePony AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

billThePony :: AllyCard BillThePony
billThePony = ally BillThePony Cards.billThePony

-- | Cannot have attachments. Lower the cost to play Bill the Pony by 2 if you control Sam
-- Gamgee. Each Hobbit character gets +1 hit point.
instance HasModifiersFor BillThePony where
  getModifiersFor (BillThePony a)
    | a.placement == Unplaced = do
        let you = PlayerWithId a.controller
        sam <- orM [selectAny (HeroControlledBy you <> HeroWithTitle "Sam Gamgee"), selectAny (AllyControlledBy you <> AllyWithTitle "Sam Gamgee")]
        when sam $ modified a (CardIdTarget a.allyCardId) [CostModifier (-2)]
    | otherwise = do
        modifySelf a [CannotHaveAttachments]
        hobbits <- selectCharacters (HeroWithTrait Hobbit) (AllyWithTrait Hobbit)
        for_ hobbits \c -> modified a c [HitPointModifier 1]
