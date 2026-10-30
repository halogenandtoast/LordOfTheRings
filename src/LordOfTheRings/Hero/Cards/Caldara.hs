module LordOfTheRings.Hero.Cards.Caldara (caldara) where

import LordOfTheRings.Card (Card (PlayerCard), CardType (AllyType), PlayerCard, cdName, cdUnique, toCardDef)
import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype Caldara = Caldara HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

caldara :: HeroCard Caldara
caldara = hero Caldara Cards.caldara

spiritAlly :: CardMatcher
spiritAlly = CardWithType AllyType <> CardWithSphere Spirit

-- | Action: Discard Caldara to put 1 spirit ally from your discard pile into play for each
-- other hero you control with a printed spirit resource icon. (Limit once per game.)
-- (FAQ 1.9 errata added the limit.)
instance HasAbilities Caldara where
  getAbilities (Caldara a) = errataOr a.owner (limited (PlayerLimit PerGame 1) rally) rally
   where
    rally =
      restricted
        a
        1
        (exists (HeroControlledBy (PlayerWithId a.owner) <> HeroWithPrintedSphere Spirit <> NotHero (HeroWithId a.id)) <> CardInDiscard a.owner spiritAlly)
        $ ActionAbility Free

instance RunMessage Caldara where
  runMessage msg s@(Caldara a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      n <- length <$> select (HeroControlledBy (PlayerWithId pid) <> HeroWithPrintedSphere Spirit <> NotHero (HeroWithId a.id))
      allies <- filterM passesUnique . filter (cardMatches spiritAlly) . (.discard) =<< getPlayer pid
      discard a
      -- each chosen at a time, the next from those the unique rule still lets in
      let pick :: ReverseQueue m => Int -> [PlayerCard] -> m ()
          pick k available = when (k > 0) $ chooseOneM pid do
            questionLabeled "caldara.chooseAlly"
            causedBy a
            targets available \pc -> do
              removeFromDiscard pid pc.id
              putAllyIntoPlayUnder pid pc
              let same c = cdUnique (toCardDef pc) && cdName (toCardDef c) == cdName (toCardDef pc)
              pick (k - 1) [c | c <- available, c.id /= pc.id, not (same c)]
      pick n [pc | PlayerCard pc <- allies]
      pure s
    _ -> Caldara <$> runMessage msg a
